from __future__ import annotations

import csv
import io
import json
import shutil
import uuid
from pathlib import Path

from fastapi import APIRouter, File, HTTPException, UploadFile
from fastapi.responses import StreamingResponse

from app.config import settings
from app.schemas import DatasetInfo, RowsRequest, RowsResponse
from app.services.data_store import dataset_store

router = APIRouter(prefix="/api/datasets", tags=["datasets"])


@router.get("", tags=["datasets"])
def list_datasets():
    """Return persisted datasets for API clients and browser-agent discovery."""
    datasets = []
    for metadata_path in sorted(settings.datasets_dir.glob("*/metadata.json"), key=lambda path: path.stat().st_mtime, reverse=True):
        try:
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
            datasets.append({key: metadata[key] for key in ("dataset_id", "name", "row_count", "columns")})
        except (OSError, KeyError, json.JSONDecodeError):
            continue
    return {"datasets": datasets}


@router.post("/upload", response_model=DatasetInfo)
async def upload_dataset(files: list[UploadFile] = File(...)) -> DatasetInfo:
    if not files:
        raise HTTPException(400, "请上传至少一个 Excel 或 CSV 文件")
    upload_dir = settings.data_dir / "uploads" / uuid.uuid4().hex
    upload_dir.mkdir(parents=True, exist_ok=True)
    paths: list[Path] = []
    try:
        for index, upload in enumerate(files):
            filename = Path(upload.filename or f"upload_{index}.csv").name
            if not filename.lower().endswith((".xlsx", ".xls", ".csv")):
                raise HTTPException(400, f"仅支持 Excel 或 CSV 文件: {filename}")
            destination = upload_dir / f"{index}_{filename}"
            size = 0
            with destination.open("wb") as handle:
                while chunk := await upload.read(1024 * 1024):
                    size += len(chunk)
                    if size > settings.max_upload_mb * 1024 * 1024:
                        raise HTTPException(413, "上传文件超过大小限制")
                    handle.write(chunk)
            paths.append(destination)
        metadata = dataset_store.create_from_files(paths, files[0].filename or "LIS 数据集")
        return DatasetInfo(**{key: metadata[key] for key in ("dataset_id", "name", "row_count", "columns", "schema", "invalid_counts")})
    except HTTPException:
        raise
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc
    finally:
        shutil.rmtree(upload_dir, ignore_errors=True)


@router.get("/{dataset_id}", response_model=DatasetInfo)
def get_dataset(dataset_id: str) -> DatasetInfo:
    try:
        metadata = dataset_store.metadata(dataset_id)
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    return DatasetInfo(**{key: metadata[key] for key in ("dataset_id", "name", "row_count", "columns", "schema", "invalid_counts")})


@router.get("/{dataset_id}/filter-options")
def filter_options(dataset_id: str):
    try:
        return {"dataset_id": dataset_id, "schema": dataset_store.filter_metadata(dataset_id)}
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc


@router.post("/{dataset_id}/rows", response_model=RowsResponse)
def get_rows(dataset_id: str, request: RowsRequest) -> RowsResponse:
    try:
        columns, rows, total = dataset_store.rows(dataset_id, request)
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc
    return RowsResponse(columns=columns, rows=rows, total=total, offset=request.offset, limit=request.limit)


@router.post("/{dataset_id}/download")
def download_rows(dataset_id: str, request: RowsRequest):
    try:
        columns, rows, _ = dataset_store.rows(dataset_id, request.model_copy(update={"offset": 0, "limit": 5000000}))
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    output = io.StringIO()
    writer = csv.DictWriter(output, fieldnames=columns, extrasaction="ignore")
    writer.writeheader()
    writer.writerows(rows)
    output.seek(0)
    return StreamingResponse(
        iter([output.getvalue().encode("utf-8-sig")]),
        media_type="text/csv; charset=utf-8",
        headers={"Content-Disposition": f'attachment; filename="lis_filtered_{dataset_id}.csv"'},
    )
