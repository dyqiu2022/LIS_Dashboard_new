from __future__ import annotations

from fastapi import APIRouter, HTTPException

import polars as pl
from app.schemas import KeywordPreviewRequest, KeywordWriteRequest, UnsupervisedRequest
from app.services.clustering import run_unsupervised
from app.services.data_store import dataset_store
from app.services.jobs import job_store, submit
from app.services.keyword import assign_keyword_classes, definition_sentence, grouping_counts

router = APIRouter(prefix="/api/clustering", tags=["clustering"])


def _run_unsupervised_request(dataset_id: str, request: UnsupervisedRequest, progress_callback=None):
    frame = dataset_store.apply_filter(dataset_id, request)
    return run_unsupervised(frame, request.grouping_col, request.cluster_num,
                            request.model_name, request.embedding_dim, request.instruction,
                            progress_callback=progress_callback)


@router.post("/{dataset_id}/keyword/counts")
def keyword_counts(dataset_id: str, request: KeywordPreviewRequest):
    try:
        frame = dataset_store.apply_filter(dataset_id, request)
        table = grouping_counts(frame, request.grouping_col)
        return {"rows": table.to_dicts(), "sentence": definition_sentence(request.definition)}
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/keyword/preview")
def keyword_preview(dataset_id: str, request: KeywordPreviewRequest):
    try:
        frame = dataset_store.apply_filter(dataset_id, request)
        if request.grouping_col not in frame.columns:
            raise ValueError(f"不存在分组列: {request.grouping_col}")
        values = frame[request.grouping_col].to_list()
        assignments = assign_keyword_classes(values, [request.definition])
        preview_frame = frame.with_columns(pl.Series("类别_关键词", assignments))
        grouped = (preview_frame.group_by([request.grouping_col, "类别_关键词"])
                   .len(name="临床诊断数量").sort("临床诊断数量", descending=True))
        categories = grouped["类别_关键词"].unique(maintain_order=True).to_list()
        traces = []
        for category in categories:
            part = grouped.filter(pl.col("类别_关键词") == category)
            traces.append({"type": "bar", "name": str(category),
                           "x": part[request.grouping_col].to_list(),
                           "y": part["临床诊断数量"].to_list()})
        return {"class_name": request.definition.class_name,
                "sentence": definition_sentence(request.definition),
                "matched_count": sum(value != "未知" for value in assignments),
                "assignments": assignments,
                "plot": {"data": traces, "layout": {"barmode": "stack", "title": "关键词聚类可视化（堆叠直方图）"}}}
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/keyword/write")
def keyword_write(dataset_id: str, request: KeywordWriteRequest):
    try:
        frame = dataset_store.frame(dataset_id)
        if request.grouping_col not in frame.columns:
            raise ValueError(f"不存在分组列: {request.grouping_col}")
        assignments = assign_keyword_classes(frame[request.grouping_col].to_list(), request.definitions)
        category = "类别_关键词"
        frame = frame.drop(category) if category in frame.columns else frame
        frame = frame.with_columns(pl.Series(category, assignments))
        metadata = dataset_store.replace_frame(dataset_id, frame)
        return {"dataset": metadata, "definitions": [d.class_name for d in request.definitions]}
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/unsupervised", response_model=None)
def unsupervised(dataset_id: str, request: UnsupervisedRequest):
    try:
        dataset_store.metadata(dataset_id)
        return submit(
            "unsupervised",
            _run_unsupervised_request,
            dataset_id,
            request,
            payload={"dataset_id": dataset_id, "request": request.model_dump()},
        )
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/unsupervised/{job_id}/write")
def write_unsupervised(dataset_id: str, job_id: str, grouping_col: str):
    try:
        job = job_store.get(job_id)
        if job.get("status") != "completed":
            raise ValueError("聚类任务尚未完成")
        result = job.get("result") or {}
        rows = result.get("rows") or []
        if not rows:
            raise ValueError("聚类结果为空")
        mapping = pl.DataFrame(rows).select([grouping_col, "类别_无监督"]).unique(subset=[grouping_col])
        frame = dataset_store.frame(dataset_id)
        if grouping_col not in frame.columns:
            raise ValueError(f"不存在聚类列: {grouping_col}")
        if "类别_无监督" in frame.columns:
            frame = frame.drop("类别_无监督")
        frame = frame.join(mapping, on=grouping_col, how="left").with_columns(pl.col("类别_无监督").fill_null("未知"))
        metadata = dataset_store.replace_frame(dataset_id, frame)
        return {"dataset": metadata}
    except KeyError as exc:
        raise HTTPException(404, "数据集或任务不存在") from exc
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.get("/jobs/{job_id}")
def get_job(job_id: str):
    try:
        return job_store.get(job_id)
    except KeyError as exc:
        raise HTTPException(404, "任务不存在") from exc
