from __future__ import annotations

import math
from typing import Any

import numpy as np
import polars as pl
from scipy import stats


LAMBDA_GRID = np.arange(-2.0, 2.0001, 0.05)


def _safe_number(value: float, digits: int = 8) -> float | int:
    value = float(value)
    rounded = round(value, digits)
    return int(rounded) if rounded.is_integer() else rounded


def boxcox_fit(values: Any, offset: float = 0.1) -> tuple[np.ndarray, float]:
    """Manual grid-search Box-Cox used by the legacy R modules."""
    raw = np.asarray(values, dtype=float)
    valid = raw[np.isfinite(raw)]
    if valid.size == 0:
        return np.full(raw.shape, np.nan), 0.0
    y = np.maximum(valid + offset, np.finfo(float).eps)
    log_y_sum = np.log(y).sum()
    likelihood = np.full(LAMBDA_GRID.shape, -np.inf)
    for i, lam in enumerate(LAMBDA_GRID):
        transformed = np.log(y) if abs(lam) < 1e-12 else (np.power(y, lam) - 1) / lam
        variance = np.var(transformed, ddof=1) if transformed.size > 1 else 0
        if variance > 0 and np.isfinite(variance):
            likelihood[i] = -transformed.size / 2 * math.log(variance) + (lam - 1) * log_y_sum
    best_lambda = float(LAMBDA_GRID[int(np.argmax(likelihood))])
    full = raw + offset
    transformed = np.full(raw.shape, np.nan)
    valid_mask = np.isfinite(full)
    if abs(best_lambda) < 1e-12:
        transformed[valid_mask] = np.log(np.maximum(full[valid_mask], np.finfo(float).eps))
    else:
        transformed[valid_mask] = (
            np.power(np.maximum(full[valid_mask], np.finfo(float).eps), best_lambda) - 1
        ) / best_lambda
    return transformed, best_lambda


def boxcox_inverse(values: Any, lam: float, offset: float = 0.1) -> np.ndarray:
    values = np.asarray(values, dtype=float)
    if abs(lam) < 1e-12:
        return np.exp(values) - offset
    base = np.maximum(lam * values + 1, 0)
    return np.power(base, 1 / lam) - offset


def _legacy_inverse_display(values: Any, lam: float) -> np.ndarray:
    """R plot labels use boxcox_rev(y, lambda) - 0.01."""
    values = np.asarray(values, dtype=float)
    if abs(lam) < 1e-12:
        return np.exp(values) - 0.01
    return np.power(np.maximum(lam * values + 1, 0), 1 / lam) - 0.01


def quantile_position(values: np.ndarray, target: float) -> float:
    valid = values[np.isfinite(values)]
    if valid.size == 0:
        return float("nan")
    return float((np.count_nonzero(valid <= target) + np.count_nonzero(valid < target)) / (2 * valid.size))


def _parse_ci(ci: list[str]) -> list[tuple[str, float]]:
    result = []
    for level in ci:
        try:
            percent = float(str(level).rstrip("%"))
        except ValueError:
            continue
        if 0 < percent < 100:
            result.append((f"{percent:g}%", percent / 100))
    return result


def _ci_for_group(group: pl.DataFrame, ci: list[str], win_width: int, min_num: int) -> list[dict[str, Any]]:
    """Port ``get_CI`` from R/fct_.R, including its integer-age windows."""
    if "年龄" not in group.columns or "boxcox_result" not in group.columns:
        return []
    age_array = group["年龄"].cast(pl.Float64, strict=False).to_numpy()
    transformed = group["boxcox_result"].cast(pl.Float64, strict=False).to_numpy()
    valid = np.isfinite(age_array) & np.isfinite(transformed)
    ages, transformed = age_array[valid], transformed[valid]
    if not len(ages):
        return []

    levels = _parse_ci(ci)
    if not levels:
        return []
    max_age = int(np.max(ages))
    # The legacy code uses max(age)-5 for its default five-year window.  Use
    # the selected width here so the same intended behavior also works for the
    # other widths exposed by the control.
    max_start = max_age - int(win_width) + 1
    starts = range(0, max_start + 1) if max_start >= 0 else range(0)
    result: list[dict[str, Any]] = []
    z975 = float(stats.norm.ppf(0.975))
    for start in starts:
        age_window = np.arange(start, start + int(win_width), dtype=float)
        mask = np.isin(ages, age_window)
        current = transformed[mask]
        if current.size < min_num:
            continue
        min_result, max_result = float(np.min(current)), float(np.max(current))
        sd = float(np.std(current, ddof=1)) if current.size > 1 else 0.0
        n = current.size
        median_age = float(np.median(age_window))
        for level, probability in levels:
            z_value = float(stats.norm.ppf(probability))
            point = float(np.quantile(current, probability, method="linear"))
            sd_ci = math.sqrt(max(sd * sd / n + (sd * sd * z_value * z_value) / (2 * n * max(n - 1, 1)), 0))
            result.append({
                "point_estimator": point,
                "CI_upper": min(point + sd_ci * z975, max_result),
                "CI_lower": max(point - sd_ci * z975, min_result),
                "年龄": median_age,
                "CI_%": level,
            })
    return result


def _rgba(color: tuple[int, int, int], alpha: float) -> str:
    return f"rgba({color[0]}, {color[1]}, {color[2]}, {alpha})"


def _group_color(index: int) -> str:
    colors = ["lightcoral", "#66CCFF", "#9AFF9A", "#FFD700", "#DA70D6"]
    return colors[index % len(colors)]


def _quantile_color(index: int) -> str:
    colors = ["brown", "darkblue", "#006400", "#8B4513", "#800080"]
    return colors[index % len(colors)]


def _rgb_css(color: str) -> tuple[int, int, int]:
    named = {"lightcoral": (240, 128, 128), "brown": (165, 42, 42), "darkblue": (0, 0, 139)}
    if color in named:
        return named[color]
    value = color.lstrip("#")
    if len(value) == 6:
        return tuple(int(value[i:i + 2], 16) for i in (0, 2, 4))
    return (80, 80, 80)


def trend(frame: pl.DataFrame, grouping_col: str, ci: list[str], win_width: int, min_num: int,
          smoothing: float = 1.3, hover_mode: str = "x unified") -> dict[str, Any]:
    required = {"年龄", "定量结果", grouping_col}
    missing = required.difference(frame.columns)
    if missing:
        raise ValueError("缺少定量趋势所需列: " + ", ".join(sorted(missing)))
    numeric = frame.with_columns(
        pl.col("年龄").cast(pl.Float64, strict=False),
        pl.col("定量结果").cast(pl.Float64, strict=False),
        pl.col(grouping_col).cast(pl.Utf8, strict=False).fill_null("NA").alias(grouping_col),
    ).filter(pl.col("年龄").is_not_null() & pl.col("定量结果").is_not_null())
    values = numeric["定量结果"].to_numpy()
    if values.size == 0:
        return {"data": [], "layout": {"title": "暂无有效定量数据"}}

    q3 = float(np.quantile(values, 0.75, method="linear"))
    q1 = float(np.quantile(values, 0.25, method="linear"))
    upper = q3 + 400 * (q3 - q1)
    clipped = np.minimum(values, upper)
    transformed, lam = boxcox_fit(clipped, offset=0.1)
    numeric = numeric.with_columns(pl.Series("boxcox_result", transformed))
    groups = numeric[grouping_col].unique(maintain_order=True).to_list()
    rows: list[dict[str, Any]] = []
    for group in groups:
        group_df = numeric.filter(pl.col(grouping_col) == group)
        for row in _ci_for_group(group_df, ci, win_width, min_num):
            rows.append({**row, "condition": str(group)})
    if not rows:
        return {"data": [], "layout": {"title": "当前条件下没有满足最小样本量的窗口"}, "lambda": lam, "rows": []}

    quantile_levels = [level for level, _ in _parse_ci(ci)]
    extreme = {quantile_levels[0], quantile_levels[-1]} if quantile_levels else set()
    traces: list[dict[str, Any]] = []
    for group_index, group in enumerate(groups):
        group_name = str(group)
        group_df = numeric.filter(pl.col(grouping_col) == group)
        color = _group_color(group_index)
        rgb = _rgb_css(color)
        raw_text: list[str] = []
        for row in group_df.to_dicts():
            sex = row.get("性别", "NA")
            diagnosis = row.get("临床诊断", "NA")
            value = float(row.get("定量结果", float("nan")))
            raw_text.append(
                f"群体: {group_name}<br>年龄: {row.get('年龄')}岁；性别: {sex}<br>"
                f"临床诊断: {diagnosis}<br>定量值: {_legacy_inverse_display([row.get('boxcox_result')], lam)[0]:.2f}"
            )
        traces.append({
            "type": "scatter", "mode": "markers", "name": f"{group_name} {group_df.height}例",
            "x": group_df["年龄"].to_list(), "y": group_df["boxcox_result"].to_list(),
            "text": raw_text, "hoverinfo": "text" if hover_mode == "constant" else "none",
            "marker": {"color": _rgba(rgb, 0.3), "size": 5},
            "legendgroup": f"{group_name}_points", "showlegend": True,
        })
        group_rows = [row for row in rows if row["condition"] == group_name]
        for q_index, level in enumerate(quantile_levels):
            part = [row for row in group_rows if row["CI_%"] == level]
            q_color = _quantile_color(group_index)  # matches the legacy group-index mapping
            percent = float(level.rstrip("%"))
            traces.append({
                "type": "scatter", "mode": "lines", "name": f"{group_name} 分位数",
                "x": [row["年龄"] for row in part], "y": [row["point_estimator"] for row in part],
                "text": [f"群体: {group_name} 年龄: {row['年龄']}岁 {level}分位数: {_legacy_inverse_display([row['point_estimator']], lam)[0]:.2f}" for row in part],
                "hoverinfo": "text", "line": {
                    "shape": "spline", "smoothing": smoothing, "color": q_color,
                    "width": 3 - 4 * abs(50 - percent) / 100,
                    "dash": "dash" if q_index % 2 == 1 else "solid",
                }, "legendgroup": group_name, "showlegend": q_index == 0,
            })
            if level in extreme:
                traces.append({
                    "type": "scatter", "mode": "markers", "x": [row["年龄"] for row in part],
                    "y": [row["point_estimator"] for row in part], "error_y": {
                        "type": "data", "symmetric": False,
                        "array": [row["CI_upper"] - row["point_estimator"] for row in part],
                        "arrayminus": [row["point_estimator"] - row["CI_lower"] for row in part],
                        "color": q_color, "thickness": 0.5, "width": 0,
                    }, "marker": {"size": 0.1, "opacity": 0},
                    "showlegend": False, "legendgroup": group_name, "hoverinfo": "none",
                })

    transformed_values = numeric["boxcox_result"].to_numpy()
    ticks = np.linspace(float(np.nanmin(transformed_values)), float(np.nanmax(transformed_values)), 8)
    return {
        "data": traces,
        "layout": {
            "title": f"{grouping_col}分层分位数趋势/散点图",
            "hovermode": hover_mode if hover_mode in {"x unified", "constant"} else "x unified",
            "hoverlabel": {"font": {"size": 14}, "bgcolor": "rgba(255,255,255,0.9)", "namelength": -1},
            "xaxis": {"title": "年龄 (岁)"},
            "yaxis": {"title": "指标水平", "tickvals": ticks.tolist(), "ticktext": [f"{x:.2f}" for x in _legacy_inverse_display(ticks, lam)]},
            "legend": {"traceorder": "grouped"},
        },
        "lambda": lam,
        "rows": rows,
    }


def _hist_color(value: float, quantiles: tuple[float, float, float, float], maximum: float) -> str:
    q05, q33, q66, q95 = quantiles
    if value <= q05:
        return "rgb(80, 0, 80)"
    if value <= q33:
        ratio = (value - q05) / max(q33 - q05, 1e-12)
        return f"rgb({int(80 - 80 * ratio)}, 0, {int(80 + 59 * ratio)})"
    if value <= q66:
        ratio = (value - q33) / max(q66 - q33, 1e-12)
        return f"rgb(0, {int(100 * ratio)}, {int(139 * (1 - ratio))})"
    ratio = (value - q66) / max(maximum - q66, 1e-12)
    return f"rgb({int(139 * ratio)}, {int(100 * (1 - ratio))}, 0)"


def histogram(frame: pl.DataFrame, column: str, cut_points: str, transform: str = "原数据",
              bins: int = 50) -> dict[str, Any]:
    from app.services.discretize import discretize, discretized_table, distribution_table, parse_cut_points

    if column not in frame.columns:
        raise ValueError(f"不存在变量: {column}")
    values = frame[column].cast(pl.Float64, strict=False).to_numpy()
    valid = values[np.isfinite(values)]
    if valid.size == 0:
        return {"data": [], "layout": {"title": "暂无有效数据"}, "distribution_table": [], "group_table": []}
    lam = None
    plotted = valid.copy()
    if transform == "Box-Cox":
        plotted, lam = boxcox_fit(valid, offset=0.1)
    count, edges = np.histogram(plotted, bins=max(1, min(int(bins), 999999)))
    centers = (edges[:-1] + edges[1:]) / 2
    quantile_values = tuple(float(np.quantile(plotted, q, method="linear")) for q in (0.05, 0.33, 0.66, 0.95))
    colors = [_hist_color(float(value), quantile_values, float(np.max(plotted))) for value in centers]
    if transform == "Box-Cox" and lam is not None:
        original_edges = _legacy_inverse_display(edges, lam)
        tickvals = np.linspace(float(np.min(plotted)), float(np.max(plotted)), 8)
        ticktext = [f"{x:.2f}" for x in _legacy_inverse_display(tickvals, lam)]
    else:
        original_edges = edges
        tickvals = np.linspace(float(np.min(plotted)), float(np.max(plotted)), 8)
        ticktext = [f"{x:.2f}" for x in tickvals]
    cdf = np.searchsorted(np.sort(plotted), edges[1:], side="right")
    hover = [
        f"数值范围: {original_edges[i]:.2f}-{original_edges[i + 1]:.2f}<br>频数: {int(count[i])}<br>"
        f"小于等于该值的样本数: {int(cdf[i])} ({cdf[i] / len(plotted) * 100:.1f}%)"
        for i in range(len(count))
    ]
    traces: list[dict[str, Any]] = [{
        "type": "bar", "x": centers.tolist(), "y": count.tolist(), "name": "频数分布",
        "text": hover, "hoverinfo": "text", "marker": {"color": colors, "line": {"color": "rgba(0,0,0,0.3)", "width": 1}},
    }]
    max_count = int(np.max(count)) if len(count) else 0
    for q, color, label in zip((0.05, 0.33, 0.66, 0.95), ("purple", "blue", "green", "red"), ("5%", "33%", "66%", "95%")):
        position = float(np.quantile(plotted, q, method="linear"))
        traces.append({"type": "scatter", "mode": "lines", "x": [position, position], "y": [0, max_count * 0.95], "line": {"color": color, "dash": "dot", "width": 1.5}, "name": f"{label}分位数", "hoverinfo": "none"})
    assigned, labels = discretize(values, parse_cut_points(cut_points, values))
    cut_info = parse_cut_points(cut_points, values)
    group_counts = [{"组名": label, "频数": int(np.count_nonzero(assigned == label)), "占比": float(np.count_nonzero(assigned == label) / valid.size)} for label in labels if np.count_nonzero(assigned == label)]
    return {
        "data": traces,
        "layout": {
            "title": f"{column}原数据频数分布直方图", "xaxis": {"title": "数值大小", "tickvals": tickvals.tolist(), "ticktext": ticktext},
            "yaxis": {"title": f"{column}原数据频数"}, "showlegend": False,
        },
        "lambda": lam, "count": int(valid.size), "distribution_table": distribution_table(values),
        "discretized_plot": {"data": [{"type": "bar", "x": [row["组名"] for row in group_counts], "y": [row["频数"] for row in group_counts], "text": [f"分组: {row['组名']}<br>频数: {row['频数']}<br>占比: {row['占比'] * 100:.1f}%" for row in group_counts], "hoverinfo": "text", "marker": {"color": "rgb(112,112,112)"}}], "layout": {"title": f"{column}分组分布直方图", "xaxis": {"title": "分组", "categoryorder": "array", "categoryarray": labels}, "yaxis": {"title": f"{column}各分组频数"}}},
        "group_table": discretized_table(values, assigned, labels, cut_info.pattern),
        "group_counts": group_counts,
    }
