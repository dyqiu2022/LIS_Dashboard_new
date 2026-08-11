from __future__ import annotations

import polars as pl
from fastapi import APIRouter, HTTPException

from app.schemas import BatchDifferenceRequest
from app.services.batch_difference import build_point_detail, run_batch_difference, validate_formula
from app.services.data_store import _json_rows, apply_filter, dataset_store
from app.services.jobs import job_store, submit

router = APIRouter(prefix="/api/batch-difference", tags=["batch-difference"])


def _run_batch_request(dataset_id: str, request: BatchDifferenceRequest, progress_callback=None):
    # Preserve full-dataset row numbers so a later point click can recover the
    # exact rows after filtering, outlier removal and complete-case selection.
    indexed = dataset_store.frame(dataset_id).with_row_index("_source_row_index")
    frame = apply_filter(indexed, request)
    result = run_batch_difference(frame, request.formula, request.n_value, request.step_value,
                                  progress_callback=progress_callback)
    result["dataset_id"] = dataset_id
    return result


@router.post("/{dataset_id}/validate")
def validate_batch_formula(dataset_id: str, request: BatchDifferenceRequest):
    try:
        metadata = dataset_store.metadata(dataset_id)
        valid, message, variables = validate_formula(request.formula, metadata["columns"])
        return {"valid": valid, "message": message, "variables": variables}
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc


@router.post("/{dataset_id}/run")
def start_batch_difference(dataset_id: str, request: BatchDifferenceRequest):
    try:
        # Validate before queueing while deferring the expensive filtering/model fit.
        metadata = dataset_store.metadata(dataset_id)
        valid, message, _ = validate_formula(request.formula, metadata["columns"])
        if not valid:
            raise HTTPException(400, message)
        job = submit("batch_difference", _run_batch_request, dataset_id, request,
                     payload={"dataset_id": dataset_id, "request": request.model_dump()})
        return job
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.get("/jobs/{job_id}")
def batch_job(job_id: str):
    try:
        return job_store.get(job_id)
    except KeyError as exc:
        raise HTTPException(404, "任务不存在") from exc


@router.get("/jobs/{job_id}/point/{point_id}")
def batch_point(job_id: str, point_id: str):
    try:
        job = job_store.get(job_id)
    except KeyError as exc:
        raise HTTPException(404, "任务不存在") from exc
    if job.get("status") != "completed":
        raise HTTPException(409, "任务尚未完成")
    result = job.get("result") or {}
    points = [row for row in result.get("all_manu_data", []) if row.get("point_id") == point_id]
    if not points:
        raise HTTPException(404, "结果点不存在")
    artifact = job_store.get_artifact(job_id) or {}
    dataset_id = result.get("dataset_id")
    raw_rows: list[dict] = []
    if dataset_id:
        try:
            indices = {int(value) for value in points[0].get("original_indices", [])}
            indexed = dataset_store.frame(dataset_id).with_row_index("_source_row_index")
            raw = indexed.filter(pl.col("_source_row_index").is_in(list(indices))).drop("_source_row_index")
            raw_rows = _json_rows(raw)
        except KeyError:
            raw_rows = []
    try:
        return build_point_detail(point_id, result, artifact, raw_rows)
    except KeyError as exc:
        raise HTTPException(404, "结果点不存在") from exc
