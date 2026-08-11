from __future__ import annotations

import re
from dataclasses import dataclass
from datetime import date, datetime
from typing import Any

import numpy as np


@dataclass(frozen=True)
class CutInfo:
    pattern: str
    points: list[float]
    labels: list[str]
    description: str


def _finite_values(values: Any) -> np.ndarray:
    array = np.asarray(values, dtype=float)
    return array[np.isfinite(array)]


def _number(value: float, digits: int = 6) -> float | int:
    value = float(value)
    rounded = round(value, digits)
    return int(rounded) if rounded.is_integer() else rounded


def parse_cut_points(text: str, values: Any) -> CutInfo:
    """Parse the legacy dashboard's ``|`` separated percentage/numeric cuts.

    The R implementation always adds 0/100% or the observed minimum/maximum,
    sorts and de-duplicates the cut positions, then assigns shared boundaries
    to the later interval.  ``discretize`` below mirrors that boundary rule.
    """
    tokens = [x.strip() for x in (text or "").split("|") if x.strip()]
    valid = _finite_values(values)
    if not tokens or valid.size == 0:
        return CutInfo("NA", [], [], "请输入正确的分段信息")

    is_percent = all(re.fullmatch(r"[-+]?\d+(?:\.\d+)?%", token) for token in tokens)
    if is_percent:
        try:
            numeric_percent = [float(token[:-1]) / 100 for token in tokens]
        except ValueError:
            numeric_percent = []
        if not numeric_percent or any(not np.isfinite(x) for x in numeric_percent):
            return CutInfo("NA", [], [], "请输入正确的分段信息，以“|”分割，支持数值或百分位数")
        points = sorted({0.0, *[x for x in numeric_percent if 0 <= x <= 1], 1.0})
        labels = [f"{points[i] * 100:g}%~{points[i + 1] * 100:g}%" for i in range(len(points) - 1)]
        return CutInfo("%", points, labels, "按分位数分组：" + ", ".join(labels))

    try:
        numeric = [float(token) for token in tokens]
    except ValueError:
        return CutInfo("NA", [], [], "请输入正确的分段信息，以“|”分割，支持数值或百分位数")
    if not numeric or any(not np.isfinite(x) for x in numeric):
        return CutInfo("NA", [], [], "请输入正确的分段信息，以“|”分割，支持数值或百分位数")
    points = sorted({float(valid.min()), *numeric, float(valid.max())})
    labels = [f"{_number(points[i]):g}~{_number(points[i + 1]):g}" for i in range(len(points) - 1)]
    return CutInfo("numeric", points, labels, "按数值分组：" + ", ".join(labels))


def discretize(values: Any, cut_info: CutInfo) -> tuple[np.ndarray, list[str]]:
    """Assign values to the same inclusive intervals as the R dashboard."""
    array = np.asarray(values, dtype=float)
    out = np.full(array.shape, None, dtype=object)
    valid = np.isfinite(array)
    if cut_info.pattern == "%":
        valid_values = array[valid]
        if valid_values.size == 0:
            return out, cut_info.labels
        cuts = np.quantile(valid_values, np.asarray(cut_info.points), method="linear")
        cuts = np.maximum.accumulate(cuts)
        positions = np.searchsorted(cuts, valid_values, side="right") - 1
        positions = np.clip(positions, 0, len(cut_info.labels) - 1)
        out[valid] = np.asarray(cut_info.labels, dtype=object)[positions]
    elif cut_info.pattern == "numeric":
        points = np.asarray(cut_info.points, dtype=float)
        positions = np.searchsorted(points, array[valid], side="right") - 1
        positions = np.clip(positions, 0, len(cut_info.labels) - 1)
        out[valid] = np.asarray(cut_info.labels, dtype=object)[positions]
    return out, cut_info.labels


def discretize_age(values: Any, grain: float = 3) -> tuple[np.ndarray, list[str]]:
    array = np.asarray(values, dtype=float)
    valid = array[np.isfinite(array)]
    if valid.size == 0:
        return np.full(array.shape, None, dtype=object), []
    grain = max(float(grain), 1e-9)
    start = np.floor(valid.min() / grain) * grain
    breaks = np.arange(start, valid.max() + grain * 1.000001, grain)
    if breaks.size < 2:
        breaks = np.array([start, start + grain])
    labels = [f"{breaks[i]:g}-{breaks[i + 1] - 1:g}" for i in range(len(breaks) - 1)]
    position = np.searchsorted(breaks, array, side="right") - 1
    position = np.clip(position, 0, len(labels) - 1)
    out = np.full(array.shape, None, dtype=object)
    out[np.isfinite(array)] = np.asarray(labels, dtype=object)[position[np.isfinite(array)]]
    return out, labels


def discretize_time(values: Any, grain: str = "month") -> tuple[np.ndarray, list[str]]:
    dates: list[datetime | date | None] = list(values)
    labels: list[str | None] = []
    for value in dates:
        if value is None:
            labels.append(None)
            continue
        if isinstance(value, date) and not isinstance(value, datetime):
            value = datetime.combine(value, datetime.min.time())
        if grain == "day":
            labels.append(value.strftime("%Y-%m-%d"))
        elif grain == "week":
            iso = value.isocalendar()
            labels.append(f"{iso.year:04d}-W{iso.week:02d}")
        else:
            labels.append(value.strftime("%Y-%m"))
    ordered = sorted({x for x in labels if x is not None})
    return np.asarray(labels, dtype=object), ordered


def distribution_table(values: Any) -> list[dict[str, Any]]:
    """Return the 0..100% by 5% table used by the legacy DT widget."""
    array = _finite_values(values)
    if array.size == 0:
        return []
    rows: list[dict[str, Any]] = []
    for percent in range(0, 101, 5):
        value = float(np.quantile(array, percent / 100, method="linear"))
        rows.append({
            "分位数": f"{percent}%",
            "数值": _number(value, 8),
            "小于等于该值的样本数": int(np.count_nonzero(array <= value)),
        })
    return rows


def discretized_table(values: Any, assigned: Any, labels: list[str], pattern: str) -> list[dict[str, Any]]:
    array = np.asarray(values, dtype=float)
    groups = np.asarray(assigned, dtype=object)
    rows: list[dict[str, Any]] = []
    for label in labels:
        mask = groups == label
        if not np.any(mask):
            continue
        row: dict[str, Any] = {"组名": label, "样本量": int(mask.sum())}
        group_values = array[mask]
        if pattern == "%":
            row["定量下界"] = _number(np.min(group_values), 2)
            row["定量上界"] = _number(np.max(group_values), 2)
        else:
            valid = array[np.isfinite(array)]
            low = (np.count_nonzero(valid < np.min(group_values)) + np.count_nonzero(valid <= np.min(group_values))) / (2 * len(valid))
            high = (np.count_nonzero(valid < np.max(group_values)) + np.count_nonzero(valid <= np.max(group_values))) / (2 * len(valid))
            row["分位数下界"] = f"{low * 100:.2f}%"
            row["分位数上界"] = f"{high * 100:.2f}%"
        rows.append(row)
    if rows and pattern != "%":
        rows[0]["分位数下界"] = "0.00%"
        rows[-1]["分位数上界"] = "100.00%"
    return rows
