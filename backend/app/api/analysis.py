from __future__ import annotations

import polars as pl
from fastapi import APIRouter, HTTPException

from app.schemas import (
    DiscretizeRequest,
    FilterRequest,
    QualitativeRequest,
    QuantitativeRequest,
)
from app.services.data_store import dataset_store
from app.services.discretize import discretize, parse_cut_points
from app.services.qualitative import consecutive_stack, discrete_stack, pie
from app.services.quantitative import histogram, trend

router = APIRouter(prefix="/api/analysis", tags=["analysis"])


def _filtered(dataset_id: str, request: FilterRequest):
    try:
        return dataset_store.apply_filter(dataset_id, request)
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc


@router.post("/{dataset_id}/pie")
def pie_chart(dataset_id: str, request: QualitativeRequest):
    frame = _filtered(dataset_id, request)
    try:
        return pie(frame, request.primary_col, request.color_mode, request.order_direction)
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/discrete-stack")
def discrete_chart(dataset_id: str, request: QualitativeRequest):
    frame = _filtered(dataset_id, request)
    if not request.secondary_col:
        raise HTTPException(400, "请选择二级分层变量")
    try:
        return discrete_stack(frame, request.primary_col, request.secondary_col, request.elements_num,
                              request.y_mode, request.color_mode, request.order_direction)
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/consecutive-stack")
def consecutive_chart(dataset_id: str, request: QualitativeRequest):
    frame = _filtered(dataset_id, request)
    try:
        return consecutive_stack(frame, request.x_var, request.primary_col, request.grain,
                                 request.y_mode, request.color_mode, request.normalize_quantitative,
                                 request.order_direction)
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/quantitative-trend")
def quantitative_chart(dataset_id: str, request: QuantitativeRequest):
    frame = _filtered(dataset_id, request)
    try:
        return trend(frame, request.grouping_col, request.ci, request.win_width, request.min_num,
                     request.smoothing, request.hover_mode)
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/histogram")
def histogram_chart(dataset_id: str, request: DiscretizeRequest):
    frame = _filtered(dataset_id, request)
    try:
        values = frame.get_column(request.column).cast(pl.Float64, strict=False)
        info = parse_cut_points(request.cut_points, values.to_numpy())
        report = histogram(frame, request.column, request.cut_points, request.transform, request.bins)
        return {"plot": report, "cut_info": {"pattern": info.pattern, "labels": info.labels, "description": info.description},
                "distribution_table": report.get("distribution_table", []),
                "discretized_plot": report.get("discretized_plot"),
                "group_table": report.get("group_table", [])}
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc


@router.post("/{dataset_id}/discretize")
def apply_discretization(dataset_id: str, request: DiscretizeRequest):
    frame = _filtered(dataset_id, request)
    if request.column not in frame.columns:
        raise HTTPException(400, f"不存在变量: {request.column}")
    try:
        values = frame.get_column(request.column).cast(pl.Float64, strict=False).to_numpy()
        info = parse_cut_points(request.cut_points, values)
        assigned, labels = discretize(values, info)
        derived = f"{request.column}_离散"
        filtered = frame.with_columns(pl.Series(derived, assigned))
        # Write a mapping back to the full dataset so filtering does not silently delete rows.
        full = dataset_store.frame(dataset_id)
        mapping = filtered.select([request.column, derived]).unique(subset=[request.column])
        full = full.drop(derived) if derived in full.columns else full
        full = full.join(mapping, on=request.column, how="left")
        metadata = dataset_store.replace_frame(dataset_id, full)
        return {"dataset": metadata, "derived_column": derived, "labels": labels,
                "description": info.description}
    except Exception as exc:
        raise HTTPException(400, str(exc)) from exc
