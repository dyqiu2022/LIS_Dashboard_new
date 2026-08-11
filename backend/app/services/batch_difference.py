from __future__ import annotations

import math
import re
from collections import Counter, defaultdict
from typing import Any

import numpy as np
import polars as pl
from scipy import stats

from app.services.quantitative import boxcox_fit, boxcox_inverse

QUANTILES = (0.1, 0.3, 0.5, 0.7, 0.9)
QUANTILE_LEVELS = tuple(f"{int(q * 100)}%" for q in QUANTILES)


def _json_scalar(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, (np.integer,)):
        return int(value)
    if isinstance(value, (np.floating,)):
        value = float(value)
        return None if not np.isfinite(value) else value
    if hasattr(value, "isoformat"):
        return value.isoformat()
    return value


def _manufacturer_column(frame: pl.DataFrame) -> pl.DataFrame:
    if "试剂厂家" in frame.columns:
        return frame.with_columns(pl.col("试剂厂家").cast(pl.Utf8, strict=False).fill_null("默认厂家").alias("试剂厂家"))
    if "医院名称" in frame.columns:
        return frame.with_columns(pl.col("医院名称").cast(pl.Utf8, strict=False).fill_null("默认厂家").alias("试剂厂家"))
    return frame.with_columns(pl.lit("默认厂家").alias("试剂厂家"))


def _formula_variables(formula: str) -> list[str]:
    expressions = re.findall(r"I\(([^)]+)\)", formula or "")
    plain = re.sub(r"I\([^)]*\)", " ", formula or "")
    tokens = re.split(r"[:+*()^\s]+", plain)
    result: list[str] = []
    for item in expressions + tokens:
        item = item.strip()
        item = re.sub(r"\^[0-9]+", "", item)
        item = re.sub(r"\*\*[0-9]+", "", item)
        if item and item not in result and item not in {"1", "0"}:
            result.append(item)
    return result


def validate_formula(formula: str, columns: list[str] | set[str]) -> tuple[bool, str, list[str]]:
    text = (formula or "").strip()
    if not text:
        return False, "请输入回归公式", []
    if "定量结果" in text:
        return False, "公式中不应包含因变量'定量结果'", []
    if not any(operator in text for operator in ("+", "*", ":")):
        return False, "公式应包含交互项或加法项", []
    variables = _formula_variables(text)
    if not variables:
        return False, "无法识别公式中的变量", []
    missing = [variable for variable in variables if variable not in columns]
    if missing:
        return False, "以下变量不在数据中: " + ", ".join(missing), variables
    return True, "公式格式正确", variables


def _mode(series: pl.Series) -> Any:
    clean = series.drop_nulls().cast(pl.Utf8, strict=False)
    if clean.len() == 0:
        return "NA"
    counts = clean.value_counts(sort=True)
    return counts[0, clean.name] if counts.height else "NA"


def _baseline(frame: pl.DataFrame, variables: list[str]) -> dict[str, Any]:
    """Port return_base_line/build_base_line_df from the Shiny module."""
    values: dict[str, Any] = {}
    unique_reagents = max(frame["试剂厂家"].n_unique(), 1)
    for variable in variables:
        if variable not in frame.columns:
            continue
        series = frame.get_column(variable)
        if variable == "年龄" and series.dtype.is_numeric():
            median = series.cast(pl.Float64, strict=False).median()
            if median is not None and np.isfinite(float(median)):
                values[variable] = float(median)
            continue
        if series.dtype.is_numeric() and variable not in {"年龄"}:
            series_for_count = series.cast(pl.Float64, strict=False)
        else:
            series_for_count = series.cast(pl.Utf8, strict=False).fill_null("NA")
        count = frame.select([pl.col(variable).alias(variable), pl.col("试剂厂家")]).with_columns(
            pl.col(variable).cast(pl.Utf8, strict=False).fill_null("NA").alias(variable),
            pl.col("试剂厂家").cast(pl.Utf8, strict=False).fill_null("NA").alias("试剂厂家"),
        ).group_by([variable, "试剂厂家"]).len(name="n")
        candidate = count.group_by(variable).agg(
            pl.col("试剂厂家").n_unique().alias("n2"), pl.col("n").min().alias("count")
        ).filter(pl.col("n2") == unique_reagents)
        if candidate.height:
            maximum = candidate["count"].max()
            candidate = candidate.filter(pl.col("count") == maximum)
            selected = candidate[variable][0]
            if series.dtype.is_numeric() and variable != "年龄":
                try:
                    values[variable] = float(selected)
                except (TypeError, ValueError):
                    values[variable] = _mode(series)
            else:
                values[variable] = str(selected)
        else:
            values[variable] = _mode(series)
    return values


def _lot_info(window: pl.DataFrame) -> str:
    columns = [column for column in ("试剂盒批号", "试剂盒盒号") if column in window.columns]
    if not columns:
        return "批号-盒号信息：未知"
    values = window.select(columns).with_columns([
        pl.col(column).cast(pl.Utf8, strict=False).fill_null("未知").str.strip_chars().replace("", "未知").alias(column)
        for column in columns
    ])
    counts = values.group_by(columns).len(name="样本数").sort("样本数", descending=True)
    parts = []
    for row in counts.to_dicts():
        parts.append("-".join(f"{'批号' if column == '试剂盒批号' else '盒号'}{row[column]}" for column in columns) + f"：{row['样本数']}例")
    return " <br>".join(parts) or "批号-盒号信息：未知"


def _bh(values: list[float]) -> list[float]:
    if not values:
        return []
    order = np.argsort(np.asarray(values, dtype=float))
    output = np.empty(len(values), dtype=float)
    running = 1.0
    for rank in range(len(values) - 1, -1, -1):
        index = int(order[rank])
        running = min(running, float(values[index]) * len(values) / (rank + 1))
        output[index] = running
    return output.tolist()


def _fit_residuals(frame: pl.DataFrame, formula: str, baseline: dict[str, Any]) -> tuple[np.ndarray, float, list[str]]:
    try:
        import pandas as pd
        import statsmodels.formula.api as smf
    except ImportError as exc:
        raise RuntimeError("批间差异分析需要 pandas 和 statsmodels") from exc
    pandas_frame = frame.to_pandas()
    variables = _formula_variables(formula)
    python_formula = re.sub(
        r"I\(([^)]*)\)", lambda match: "I(" + match.group(1).replace("^", "**") + ")", formula
    )
    model_formula = f"定量结果_transformed ~ {python_formula}"
    try:
        model = smf.ols(model_formula, data=pandas_frame).fit()
    except Exception as exc:
        raise ValueError(f"回归公式无法拟合: {exc}") from exc
    baseline_df = pd.DataFrame([{key: value for key, value in baseline.items() if key in pandas_frame.columns}])
    try:
        prediction = float(model.predict(baseline_df).iloc[0])
    except Exception as exc:
        raise ValueError(f"无法计算基线预测值: {exc}") from exc
    return np.asarray(model.resid, dtype=float), prediction, variables


def _prepare_frame(frame: pl.DataFrame) -> tuple[pl.DataFrame, float]:
    frame = _manufacturer_column(frame)
    if "_source_row_index" not in frame.columns:
        frame = frame.with_row_index("_source_row_index")
    frame = frame.with_columns(
        pl.col("定量结果").cast(pl.Float64, strict=False),
        pl.col("采样时间").cast(pl.Datetime, strict=False),
    ).filter(pl.col("定量结果").is_not_null())
    values = frame["定量结果"].to_numpy()
    if values.size == 0:
        raise ValueError("没有有效的定量结果")
    q1, q3 = np.quantile(values, [0.25, 0.75], method="linear")
    iqr = float(q3 - q1)
    lower, upper = float(q1 - 100 * iqr), float(q3 + 100 * iqr)
    frame = frame.filter((pl.col("定量结果") >= lower) & (pl.col("定量结果") <= upper))
    frame = frame.sort("采样时间", nulls_last=True)
    values = frame["定量结果"].to_numpy()
    transformed, lam = boxcox_fit(values, offset=0.01)
    return frame.with_columns(pl.Series("定量结果_transformed", transformed)), lam


def run_batch_difference(frame: pl.DataFrame, formula: str, n_value: int, step_value: int,
                         progress_callback=None) -> dict[str, Any]:
    if progress_callback:
        progress_callback(0.02, "正在校验批间差异参数")
    required = {"定量结果", "采样时间"}
    missing = required.difference(frame.columns)
    if missing:
        raise ValueError("缺少批间差异所需列: " + ", ".join(sorted(missing)))
    valid, message, formula_vars = validate_formula(formula, frame.columns)
    if not valid:
        raise ValueError(message)
    frame, lam = _prepare_frame(frame)
    if progress_callback:
        progress_callback(0.08, "数据预处理完成，正在拟合基线")
    baseline = _baseline(frame, formula_vars)
    results: list[dict[str, Any]] = []
    manufacturer_payload: dict[str, Any] = {}
    manufacturer_details: dict[str, Any] = {}
    pvalue_groups: dict[tuple[str, str], list[dict[str, Any]]] = defaultdict(list)
    step = max(int(step_value), 1)
    requested_n = int(n_value)

    manufacturers = frame["试剂厂家"].unique(maintain_order=True).to_list()
    total_manufacturers = max(len(manufacturers), 1)
    for manufacturer_index, manufacturer in enumerate(manufacturers):
        manufacturer_name = str(manufacturer)
        if progress_callback:
            progress_callback(0.12 + 0.76 * manufacturer_index / total_manufacturers,
                              f"正在分析厂家 {manufacturer_name}")
        subset = frame.filter(pl.col("试剂厂家") == manufacturer)
        if subset.height < 3:
            continue
        complete_columns = ["定量结果_transformed", *[variable for variable in formula_vars if variable in subset.columns]]
        # The Shiny module selects available lot/box columns before
        # complete.cases(), so preserve that legacy filtering behavior.
        complete_columns.extend(column for column in ("试剂盒批号", "试剂盒盒号") if column in subset.columns)
        subset = subset.drop_nulls(complete_columns)
        if subset.height < 3:
            continue
        residuals, baseline_prediction, variables = _fit_residuals(subset, formula, baseline)
        if residuals.size == 0:
            continue
        subset = subset.with_columns(pl.Series("回归残差", residuals))
        population_sorted = np.sort(residuals)
        manufacturer_details[manufacturer_name] = {
            "row_indices": [int(x) for x in subset["_source_row_index"].to_list()],
            "residuals": [float(x) for x in residuals],
            "original_values": [float(x) for x in subset["定量结果"].to_list()],
            "covariates": {
                variable: [_json_scalar(x) for x in subset[variable].to_list()]
                for variable in formula_vars if variable in subset.columns
            },
        }
        manufacturer_payload[manufacturer_name] = {
            "variables": variables, "baseline": {key: _json_scalar(value) for key, value in baseline.items()},
            "baseline_prediction": float(baseline_prediction), "row_count": subset.height,
        }
        if subset.height <= requested_n:
            continue
        win_num = (subset.height - requested_n) // step
        for quantile_index, (q, level) in enumerate(zip(QUANTILES, QUANTILE_LEVELS)):
            k = int(round(requested_n * q))
            if k < 1:
                continue
            for index in range(win_num + 1):
                start = index * step
                stop = start + requested_n  # zero-based exclusive; R uses start:(start+n)
                if stop >= subset.height:
                    break
                window = subset.slice(start, requested_n + 1)
                testing = residuals[start:stop + 1]
                if len(testing) < k:
                    continue
                x_k = float(np.sort(testing)[min(k, len(testing)) - 1])
                outside = np.concatenate((residuals[:start], residuals[stop + 1:]))
                if outside.size == 0:
                    continue
                u = float(np.count_nonzero(outside <= x_k) / outside.size)
                probability = float(stats.beta.cdf(u, k, requested_n - k + 1))
                if not np.isfinite(probability):
                    continue
                p_value = min(probability, 1 - probability)
                q_residual = float(np.quantile(testing, q, method="linear"))
                outside_q = float(np.quantile(outside, q, method="linear"))
                baseline_level = float(boxcox_inverse([outside_q + baseline_prediction], lam, offset=0.01)[0])
                equivalent = float(boxcox_inverse([q_residual + baseline_prediction], lam, offset=0.01)[0])
                start_one, stop_one = start + 1, stop + 1
                row = {
                    "point_id": "",
                    "manu_name": manufacturer_name,
                    "quantile_level": level,
                    "win_sample_start": start_one,
                    "win_sample_stop": stop_one,
                    "start_time": _json_scalar(window["采样时间"][0]),
                    "stop_time": _json_scalar(window["采样时间"][-1]),
                    "quantile_statistics": q_residual,
                    "p_value": p_value,
                    "p_value_bonferroni": p_value,
                    "等效基线水平": baseline_level,
                    "等效水平": equivalent,
                    "等效波动": equivalent - baseline_level,
                    "original_indices": [int(x) for x in window["_source_row_index"].to_list()],
                    "batch_lot_info": _lot_info(window),
                    "mid_point": (start_one + stop_one) / 2,
                }
                pvalue_groups[(manufacturer_name, level)].append(row)
                results.append(row)
                if progress_callback and (index % 10 == 0 or index == win_num):
                    within_manufacturer = (quantile_index * (win_num + 1) + index + 1) / (len(QUANTILES) * (win_num + 1))
                    overall = (manufacturer_index + within_manufacturer) / total_manufacturers
                    progress_callback(0.12 + 0.76 * overall,
                                      f"正在分析 {manufacturer_name} · {level} · 窗口 {index + 1}/{win_num + 1}")

    if progress_callback:
        progress_callback(0.91, "正在校正显著性并整理结果")
    # R adjusts p-values separately for each manufacturer/quantile result table.
    for rows in pvalue_groups.values():
        adjusted = _bh([float(row["p_value"]) for row in rows])
        for row, value in zip(rows, adjusted):
            row["p_value_bonferroni"] = round(float(value), 7)
    sequence: dict[tuple[str, str], int] = defaultdict(int)
    for row in results:
        key = (row["manu_name"], row["quantile_level"])
        sequence[key] += 1
        row["point_id"] = f"{row['manu_name']}_{row['quantile_level']}_{sequence[key]}"
        row["hover_text"] = (
            f"厂家: {row['manu_name']}<br>分位数: {row['quantile_level']}<br>"
            f"窗口: {row['win_sample_start']}-{row['win_sample_stop']}<br>"
            f"p-value: {row['p_value_bonferroni']:.4g}<br>等效水平: {row['等效水平']:.4g}<br>"
            f"等效基线水平: {row['等效基线水平']:.4g}<br>等效波动: {row['等效波动']:.4g}<br>点击查看原始数据"
        )
    results.sort(key=lambda row: (str(row["manu_name"]), row["win_sample_start"], QUANTILE_LEVELS.index(row["quantile_level"])))

    stats_summary: list[dict[str, Any]] = []
    for (manufacturer, level), rows in pvalue_groups.items():
        changes = np.asarray([float(row["等效波动"]) for row in rows])
        q1, q3 = np.quantile(changes, [0.25, 0.75], method="linear") if len(changes) else (np.nan, np.nan)
        stats_summary.append({
            "manu_name": manufacturer, "quantile_level": level,
            "Q1": float(q1), "Q3": float(q3), "IQR_val": float(q3 - q1),
            "Min": float(np.min(changes)), "Max": float(np.max(changes)), "Range_val": float(np.max(changes) - np.min(changes)),
            "error_rate": float(np.mean([float(row["p_value_bonferroni"]) <= 0.05 for row in rows])),
        })

    window_all_green: dict[str, bool] = {}
    control_original: list[float] = []
    control_residuals: list[float] = []
    control_covariates: dict[str, list[Any]] = defaultdict(list)
    for manufacturer, details in manufacturer_details.items():
        by_window: dict[tuple[int, int], list[dict[str, Any]]] = defaultdict(list)
        for row in results:
            if row["manu_name"] == manufacturer:
                by_window[(row["win_sample_start"], row["win_sample_stop"])].append(row)
        for (start, stop), rows in by_window.items():
            key = f"{manufacturer}_{start}_{stop}"
            is_green = len(rows) == len(QUANTILES) and all(float(row["p_value_bonferroni"]) > 0.05 for row in rows)
            window_all_green[key] = is_green
            if is_green:
                lo, hi = start - 1, stop
                control_original.extend(details["original_values"][lo:hi])
                control_residuals.extend(details["residuals"][lo:hi])
                for variable, values in details["covariates"].items():
                    control_covariates[variable].extend(values[lo:hi])

    artifact = {
        "manufacturers": manufacturer_details,
        "control_group_original": control_original,
        "control_group_residuals": control_residuals,
        "control_group_covariates": dict(control_covariates),
        "window_all_green_map": window_all_green,
        "formula_variables": formula_vars,
        "regression_formula": formula,
    }
    if progress_callback:
        progress_callback(0.98, f"已生成 {len(results)} 个结果点")
    return {
        "all_manu_data": results,
        "stats_summary": stats_summary,
        "best_lambda": lam,
        "manufacturers": manufacturer_payload,
        "window_all_green_map": window_all_green,
        "formula": formula,
        "n_value": requested_n,
        "step_value": int(step_value),
        "quantile_levels": list(QUANTILE_LEVELS),
        "_artifact": artifact,
    }


def interpret_point(result: dict[str, Any], all_results: list[dict[str, Any]]) -> str:
    manufacturer = result.get("manu_name", "")
    start = result.get("win_sample_start")
    stop = result.get("win_sample_stop")
    batch = [row for row in all_results if row.get("manu_name") == manufacturer and row.get("win_sample_start") == start and row.get("win_sample_stop") == stop]
    batch = sorted(batch, key=lambda item: QUANTILE_LEVELS.index(item.get("quantile_level", "50%")) if item.get("quantile_level") in QUANTILE_LEVELS else 99)
    if not batch:
        return "无法获取该窗口的分位数数据。"
    descriptions: list[str] = []
    patterns: list[str] = []
    for row in batch:
        p_value = float(row.get("p_value_bonferroni", 1))
        change = float(row.get("等效波动", 0))
        if p_value <= 0.05:
            pattern = "显著升高" if change > 0 else "显著降低"
        elif abs(change) < 0.01:
            pattern = "正常"
        else:
            pattern = "升高" if change > 0 else "降低"
        patterns.append(pattern)
        descriptions.append(f"{row.get('quantile_level')}分位数{pattern}（等效波动：{change:.4g}，p值：{p_value:.4g}）")

    significant_up = [row.get("quantile_level") for row, pattern in zip(batch, patterns) if pattern == "显著升高"]
    significant_down = [row.get("quantile_level") for row, pattern in zip(batch, patterns) if pattern == "显著降低"]
    up = [row.get("quantile_level") for row, pattern in zip(batch, patterns) if pattern == "升高"]
    down = [row.get("quantile_level") for row, pattern in zip(batch, patterns) if pattern == "降低"]
    if len(significant_up) == len(batch) and len(batch) == 5:
        conclusion = "该batch的所有分位数均显著升高，提示存在系统性正偏倚。"
        detail = "所有分位数的一致性显著升高表明这是系统性的正偏倚，而非偶然波动。"
        warning = "建议检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
    elif len(significant_down) == len(batch) and len(batch) == 5:
        conclusion = "该batch的所有分位数均显著降低，提示存在系统性负偏倚。"
        detail = "所有分位数的一致性显著降低表明这是系统性的负偏倚，而非偶然波动。"
        warning = "建议检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
    elif significant_up and significant_down:
        low_up = any(level in {"10%", "30%"} for level in significant_up)
        high_down = any(level in {"70%", "90%"} for level in significant_down)
        low_down = any(level in {"10%", "30%"} for level in significant_down)
        high_up = any(level in {"70%", "90%"} for level in significant_up)
        if low_up and high_down:
            conclusion = "该batch呈现显著的浓度依赖型变化，提示数据分布发生压缩或非线性响应。"
            detail = "低分位数显著升高而高分位数显著降低，可能提示线性范围问题或Hook效应。"
        elif low_down and high_up:
            conclusion = "该batch呈现显著的浓度依赖型变化，提示数据分布发生扩展或精密度变化。"
            detail = "低分位数显著降低而高分位数显著升高，可能提示精密度问题或反应体系不稳定。"
        else:
            conclusion = "该batch存在显著的分布变化，不同浓度区间表现不一致。"
            detail = "建议结合校准曲线、线性验证结果和质控数据进一步定位。"
        warning = "建议检查校准曲线、线性验证结果、质控数据和试剂稳定性。"
    elif significant_up:
        conclusion = f"该batch的{'、'.join(significant_up)}分位数显著升高，提示存在正偏倚趋势。"
        detail = "建议检查试剂批号、校准状态和质控结果。"
        warning = "如显著变化持续或扩大，应评估批间一致性和生产记录。"
    elif significant_down:
        conclusion = f"该batch的{'、'.join(significant_down)}分位数显著降低，提示存在负偏倚趋势。"
        detail = "建议检查试剂批号、校准状态和质控结果。"
        warning = "如显著变化持续或扩大，应评估批间一致性和生产记录。"
    elif up or down:
        conclusion = f"该batch存在{'、'.join(up + down)}等非显著变化趋势，但未达到统计学显著水平。"
        detail = "变化可能处于正常波动范围，建议持续监控。"
        warning = ""
    else:
        conclusion = "该batch的所有分位数均保持正常，未检测到显著的系统性偏差。"
        detail = "所有分位数均在正常范围内，该批次表现稳定。"
        warning = ""
    return (f"厂家：{manufacturer}<br>窗口：{start}-{stop}<br>"
            f"分位数表现：{'；'.join(descriptions)}。<br>"
            f"<strong>综合结论：{conclusion}</strong><br>{detail}"
            + (f"<br><span class='batch-warning'>提示：{warning}</span>" if warning else ""))


def _density(values: list[float], name: str, color: str) -> dict[str, Any] | None:
    clean = np.asarray([float(value) for value in values if value is not None and np.isfinite(float(value))], dtype=float)
    if clean.size < 2:
        return None
    if np.allclose(clean, clean[0]):
        grid = np.linspace(clean[0] - 0.5, clean[0] + 0.5, 80)
        density = np.zeros_like(grid)
        density[len(grid) // 2] = 1
    else:
        grid = np.linspace(float(clean.min()), float(clean.max()), 120)
        density = stats.gaussian_kde(clean)(grid)
    if color.startswith("rgba("):
        channels = color[5:-1].split(",")[:3]
        fill = f"rgba({','.join(channels)}, 0.25)"
    else:
        fill = color
    return {"type": "scatter", "mode": "lines", "fill": "tozeroy", "x": grid.tolist(), "y": density.tolist(), "name": name, "line": {"color": color, "width": 2}, "fillcolor": fill}


def build_point_detail(point_id: str, result: dict[str, Any], artifact: dict[str, Any], original_rows: list[dict[str, Any]]) -> dict[str, Any]:
    points = [row for row in result.get("all_manu_data", []) if row.get("point_id") == point_id]
    if not points:
        raise KeyError(point_id)
    point = points[0]
    manufacturer = str(point["manu_name"])
    details = artifact.get("manufacturers", {}).get(manufacturer, {})
    start, stop = int(point["win_sample_start"]), int(point["win_sample_stop"])
    test_values = details.get("original_values", [])[start - 1:stop]
    control_values = artifact.get("control_group_original", [])
    density = [trace for trace in (
        _density(control_values, "全绿通过数据", "rgba(0, 180, 0, 0.8)"),
        _density(test_values, "测试窗口数据", "rgba(220, 0, 0, 0.8)"),
    ) if trace]
    enrichment = _enrichment(details, artifact, start, stop)
    window_key = f"{manufacturer}_{start}_{stop}"
    return {
        "point": point,
        "interpretation": interpret_point(point, result.get("all_manu_data", [])),
        "is_all_green": bool(artifact.get("window_all_green_map", {}).get(window_key, False)),
        "raw_rows": original_rows,
        "density": {"data": density, "layout": {"title": "核密度估计 - 分布对比", "xaxis": {"title": "定量结果（原数据）"}, "yaxis": {"title": "概率密度"}, "hovermode": "x unified"}},
        "enrichment": enrichment,
        "batch_lot_info": point.get("batch_lot_info", "批号-盒号信息：未知"),
    }


def _enrichment(details: dict[str, Any], artifact: dict[str, Any], start: int, stop: int) -> list[dict[str, Any]]:
    control = artifact.get("control_group_covariates", {})
    variables = artifact.get("formula_variables", [])
    output: list[dict[str, Any]] = []
    for variable in variables:
        testing = details.get("covariates", {}).get(variable, [])[start - 1:stop]
        population = control.get(variable, [])
        testing = [value for value in testing if value is not None]
        population = [value for value in population if value is not None]
        if not testing or not population:
            continue
        numeric_testing = _as_numeric(testing)
        numeric_population = _as_numeric(population)
        if numeric_testing is not None and numeric_population is not None:
            normal_testing = len(testing) <= 5000 and _shapiro_normal(numeric_testing)
            normal_population = len(population) <= 5000 and _shapiro_normal(numeric_population)
            if normal_testing and normal_population:
                p_value = float(stats.ttest_ind(numeric_testing, numeric_population, equal_var=False, nan_policy="omit").pvalue)
                method = "t检验"
            else:
                p_value = float(stats.mannwhitneyu(numeric_testing, numeric_population, alternative="two-sided").pvalue)
                method = "Wilcoxon检验"
            output.append({"协变量": variable, "类型": "数值型", "检验方法": method,
                           "测试组": f"{np.mean(numeric_testing):.4f}", "对照组": f"{np.mean(numeric_population):.4f}",
                           "差异": f"{np.mean(numeric_testing) - np.mean(numeric_population):.4f}", "p_value": p_value})
        else:
            test_counts = Counter(str(value) for value in testing)
            population_counts = Counter(str(value) for value in population)
            levels = sorted(set(test_counts) | set(population_counts))
            table = np.asarray([[test_counts[level] for level in levels], [population_counts[level] for level in levels]], dtype=float)
            if len(levels) < 2:
                continue
            try:
                chi2, chi_p, _, expected = stats.chi2_contingency(table)
            except ValueError:
                continue
            if np.all(expected >= 5):
                p_value, method = float(chi_p), "卡方检验"
            elif table.shape == (2, 2):
                p_value, method = float(stats.fisher_exact(table)[1]), "Fisher精确检验"
            else:
                continue
            test_prop = table[0] / max(table[0].sum(), 1)
            population_prop = table[1] / max(table[1].sum(), 1)
            index = int(np.argmax(np.abs(test_prop - population_prop)))
            output.append({"协变量": variable, "类型": "分类变量", "检验方法": method,
                           "测试组": f"{test_prop[index] * 100:.2f}%", "对照组": f"{population_prop[index] * 100:.2f}%",
                           "差异": f"{(test_prop[index] - population_prop[index]) * 100:.2f}%", "p_value": p_value})
    adjusted = _bh([float(row["p_value"]) for row in output])
    for row, value in zip(output, adjusted):
        row["p_value_adjusted"] = round(float(value), 6)
        row["显著"] = "是" if value < 0.05 else "否"
        row["p_value"] = round(float(row["p_value"]), 6)
    return sorted(output, key=lambda row: row.get("p_value_adjusted", 1))


def _as_numeric(values: list[Any]) -> np.ndarray | None:
    try:
        array = np.asarray([float(value) for value in values], dtype=float)
    except (TypeError, ValueError):
        return None
    return array if np.all(np.isfinite(array)) else None


def _shapiro_normal(values: np.ndarray) -> bool:
    if values.size < 3:
        return False
    try:
        return bool(stats.shapiro(values[:5000]).pvalue > 0.05)
    except Exception:
        return False
