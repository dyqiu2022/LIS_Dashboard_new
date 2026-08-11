from __future__ import annotations

from typing import Any

import numpy as np
import polars as pl

from app.services.colors import palette
from app.services.discretize import discretize_age, discretize_time
from app.services.quantitative import boxcox_fit, boxcox_inverse


def _as_text(frame: pl.DataFrame, columns: list[str]) -> pl.DataFrame:
    return frame.with_columns([
        pl.col(column).cast(pl.Utf8, strict=False).fill_null("NA").alias(column)
        for column in columns
    ])


def _counts(frame: pl.DataFrame, columns: list[str]) -> pl.DataFrame:
    return _as_text(frame, columns).group_by(columns).len(name="数量")


def pie(frame: pl.DataFrame, column: str, color_mode: str = "默认颜色", order_direction: int = 1) -> dict[str, Any]:
    if column not in frame.columns:
        raise ValueError(f"不存在分层列: {column}")
    clean = _as_text(frame.select(column), [column])
    # R uses order(order_para * freq): +1 is ascending, -1 descending.
    data = clean.group_by(column).len(name="频数").sort("频数", descending=order_direction < 0)
    values = [str(value) for value in data[column].to_list()]
    frequencies = [int(value) for value in data["频数"].to_list()]
    total = max(sum(frequencies), 1)
    labels = [f"{value} 例数:{freq} 占比:{freq / total:.1%}" for value, freq in zip(values, frequencies)]
    return {
        "data": [{"type": "pie", "labels": labels, "values": frequencies, "textinfo": "none",
                  "hoverinfo": "label", "sort": False, "marker": {"colors": palette(values, color_mode)}}],
        "layout": {"title": f"各{column}数量/占比", "showlegend": True, "margin": {"t": 40}},
    }


def discrete_stack(frame: pl.DataFrame, primary: str, secondary: str, elements_num: int,
                   mode: str, color_mode: str, order_direction: int = 1) -> dict[str, Any]:
    if primary not in frame.columns or secondary not in frame.columns:
        raise ValueError("定性分析列不存在")
    if primary == secondary:
        raise ValueError("一级/二级变量不能相同")
    grouped = _counts(frame, [primary, secondary])
    # R: rank secondary values within each primary by descending count, then
    # order the final primary categories by total descending.
    ranked = (
        grouped.sort([primary, "数量"], descending=[False, True])
        .with_columns(pl.col("数量").cum_count().over(primary).alias("rank"))
        .with_columns(
            pl.when(pl.col("rank") <= int(elements_num))
            .then(pl.col(secondary).cast(pl.Utf8))
            .otherwise(pl.lit("其他"))
            .alias("类别")
        )
        .group_by([primary, "类别"])
        .agg(pl.col("数量").sum())
        .with_columns(pl.col("数量").sum().over(primary).alias("总数"))
        .with_columns((pl.col("数量") / pl.col("总数")).alias("比例"))
        .sort(["总数", "数量"], descending=[True, order_direction > 0])
    )
    x_values = [str(value) for value in ranked[primary].unique(maintain_order=True).to_list()]
    categories = [str(x) for x in ranked["类别"].unique(maintain_order=True).to_list() if str(x) != "其他"]
    if "其他" in ranked["类别"].to_list():
        categories.append("其他")
    traces = []
    for category, color in zip(categories, palette(categories, color_mode)):
        part = ranked.filter(pl.col("类别") == category)
        lookup = {str(row[primary]): row for row in part.to_dicts()}
        y_key = "数量" if mode == "数量" else "比例"
        custom = [[lookup.get(x, {}).get("数量", 0), lookup.get(x, {}).get("比例", 0)] for x in x_values]
        traces.append({"type": "bar", "name": category, "x": x_values,
                       "y": [lookup.get(x, {}).get(y_key, 0) for x in x_values],
                       "marker": {"color": color}, "customdata": custom,
                       "hovertemplate": f"一级分层变量: %{{x}}<br>二级分层变量: {category}<br>数量: %{{customdata[0]}} (%{{customdata[1]:.1%}})<extra></extra>"})
    return {
        "data": traces,
        "layout": {"barmode": "stack", "title": f"{primary} vs {secondary}",
                    "xaxis": {"title": primary, "categoryorder": "array", "categoryarray": x_values},
                    "yaxis": {"title": mode, "tickformat": ".0%" if mode == "比例" else ""},
                    "legend": {"title": {"text": secondary}}},
    }


def _numeric_bins(values: np.ndarray, grain: float) -> tuple[np.ndarray, list[str], np.ndarray]:
    valid = values[np.isfinite(values)]
    if valid.size == 0:
        return np.full(values.shape, None, dtype=object), [], np.array([])
    grain = float(grain)
    if not np.isfinite(grain) or grain <= 0:
        raise ValueError("颗粒度必须是一个有限的正数")
    minimum, maximum = float(np.min(valid)), float(np.max(valid))
    start = np.floor(minimum / grain) * grain
    n_bins = int(np.ceil((maximum - start) / grain))
    if n_bins == 0:
        breaks = np.array([start, maximum])
    else:
        breaks = start + np.arange(n_bins + 1, dtype=float) * grain
        if breaks[-1] > maximum + grain * 0.5:
            breaks[-1] = maximum
        elif breaks[-1] < maximum:
            breaks = np.append(breaks, maximum)
    if len(breaks) < 2 or breaks[-1] == breaks[0]:
        breaks = np.array([start, maximum if maximum > start else start + grain])
    lowers = breaks[:-1]
    uppers = breaks[1:].copy()
    uppers[-1] = maximum
    labels: list[str] = []
    for index, (lower, upper) in enumerate(zip(lowers, uppers)):
        if grain >= 1:
            shown_upper = maximum if index == len(uppers) - 1 else upper - 1
            labels.append(f"{lower:g}-{shown_upper:g}")
        else:
            shown_upper = maximum if index == len(uppers) - 1 else round(upper - 0.001, 3)
            labels.append(f"{round(lower, 3):g}-{round(shown_upper, 3):g}")
    position = np.searchsorted(breaks, values, side="right") - 1
    position = np.clip(position, 0, len(labels) - 1)
    output = np.full(values.shape, None, dtype=object)
    output[np.isfinite(values)] = np.asarray(labels, dtype=object)[position[np.isfinite(values)]]
    return output, labels, breaks


def consecutive_stack(frame: pl.DataFrame, x_var: str, group_var: str, grain: str | float,
                       mode: str, color_mode: str, normalize: bool = False,
                       order_direction: int = 1) -> dict[str, Any]:
    if group_var not in frame.columns:
        raise ValueError(f"不存在分层列: {group_var}")
    if x_var not in {"年龄", "采样时间", "定量结果"}:
        raise ValueError(f"不支持的连续变量: {x_var}")

    working = frame
    original_bin_labels: dict[str, str] = {}
    if x_var == "年龄":
        bins, levels = discretize_age(frame.get_column("年龄").cast(pl.Float64, strict=False).to_numpy(), float(grain))
        x_label = "年龄"
    elif x_var == "采样时间":
        bins, levels = discretize_time(frame.get_column("采样时间").to_list(), str(grain))
        x_label = "时间"
    else:
        values = frame.get_column("定量结果").cast(pl.Float64, strict=False).to_numpy()
        transformed = values
        lam = None
        if normalize:
            transformed, lam = boxcox_fit(values, offset=0.1)
        bins, levels, _ = _numeric_bins(transformed, float(grain))
        if normalize and lam is not None:
            # The chart bins are in transformed space, but the legacy UI labels
            # them with the inverse-transformed original values.
            for label in levels:
                parts = label.split("-", 1)
                if len(parts) == 2:
                    try:
                        left, right = map(float, parts)
                        original_bin_labels[label] = f"{boxcox_inverse([left], lam)[0]:.2f}-{boxcox_inverse([right], lam)[0]:.2f}"
                    except ValueError:
                        pass
        x_label = "定量结果（Box-Cox转换后，横坐标为原值）" if normalize else "定量结果"

    temp = _as_text(frame.select(group_var), [group_var]).with_columns(pl.Series("分组", bins))
    temp = temp.filter(pl.col("分组").is_not_null())
    if temp.height == 0:
        return {"data": [], "layout": {"title": "暂无可用数据"}}
    grouped = temp.group_by(["分组", group_var]).len(name="数量")
    totals = grouped.group_by("分组").agg(pl.col("数量").sum().alias("总数"))
    grouped = grouped.join(totals, on="分组").with_columns((pl.col("数量") / pl.col("总数")).alias("比例"))
    group_totals = grouped.group_by(group_var).agg(pl.col("数量").sum().alias("总数"))
    group_totals = group_totals.sort("总数", descending=order_direction < 0)
    groups = [str(x) for x in group_totals[group_var].to_list()]
    x_values = [x for x in levels if x in set(str(v) for v in grouped["分组"].to_list())]
    traces = []
    for group, color in zip(groups, palette(groups, color_mode)):
        part = grouped.filter(pl.col(group_var).cast(pl.Utf8) == group)
        lookup = {str(row["分组"]): row for row in part.to_dicts()}
        y_key = "数量" if mode == "数量" else "比例"
        traces.append({"type": "bar", "name": group, "x": x_values,
                       "y": [lookup.get(x, {}).get(y_key, 0) for x in x_values],
                       "marker": {"color": color},
                       "customdata": [[lookup.get(x, {}).get("数量", 0), lookup.get(x, {}).get("比例", 0)] for x in x_values],
                       "hovertemplate": "分组: %{x}<br>数量: %{customdata[0]} (%{customdata[1]:.1%})<extra></extra>"})
    ticktext = [original_bin_labels.get(x, x) for x in x_values]
    return {
        "data": traces,
        "layout": {"barmode": "stack", "title": f"患者{'数量' if mode == '数量' else '数量占比'}堆叠图（{x_label}）",
                    "xaxis": {"title": x_label, "categoryorder": "array", "categoryarray": x_values, "ticktext": ticktext, "tickvals": x_values},
                    "yaxis": {"title": "统计数量" if mode == "数量" else "占比", "tickformat": ".0%" if mode != "数量" else ""}},
    }
