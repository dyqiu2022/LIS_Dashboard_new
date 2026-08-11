from __future__ import annotations

import hashlib
import io
import json
import math
import threading
import uuid
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any, Iterable

import polars as pl

from app.config import settings
from app.schemas import FilterRequest, RowsRequest


NA_MARKERS = {"", "NA", "N/A", "NaN", "NULL", "null", "None", "结果无效", "无效"}
DATE_COLUMNS = {"采样时间", "检测时间", "报告时间"}
NUMERIC_COLUMNS = {"年龄", "定量结果"}


def _clean_text_expr(expr: pl.Expr) -> pl.Expr:
    cleaned = expr.cast(pl.Utf8, strict=False).str.strip_chars()
    return pl.when(cleaned.is_in(list(NA_MARKERS))).then(pl.lit(None)).otherwise(cleaned)


def _parse_excel_date(value: Any) -> datetime | None:
    if value is None:
        return None
    if isinstance(value, datetime):
        return value
    if isinstance(value, date):
        return datetime.combine(value, datetime.min.time())
    text = str(value).strip()
    if not text or text in NA_MARKERS:
        return None
    try:
        number = float(text)
        if math.isfinite(number) and 1 <= number <= 100000:
            return datetime(1899, 12, 30) + timedelta(days=number)
    except (TypeError, ValueError):
        pass
    for fmt in (
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d",
        "%Y/%m/%d %H:%M:%S",
        "%Y/%m/%d %H:%M",
        "%Y/%m/%d",
        "%Y.%m.%d",
    ):
        try:
            return datetime.strptime(text, fmt)
        except ValueError:
            continue
    try:
        return datetime.fromisoformat(text.replace("Z", "+00:00")).replace(tzinfo=None)
    except ValueError:
        return None


def _json_value(value: Any) -> Any:
    if value is None:
        return "NA"
    if isinstance(value, (datetime, date)):
        return value.isoformat(sep=" ") if isinstance(value, datetime) else value.isoformat()
    if isinstance(value, float) and (math.isnan(value) or math.isinf(value)):
        return "NA"
    return value


def _json_rows(frame: pl.DataFrame) -> list[dict[str, Any]]:
    return [{key: _json_value(value) for key, value in row.items()} for row in frame.to_dicts()]


def _read_excel(path: Path) -> pl.DataFrame:
    """Read Excel without pandas; calamine is substantially lighter for normal xlsx files."""
    try:
        return pl.read_excel(path, engine="calamine")
    except Exception as calamine_error:
        try:
            return pl.read_excel(path, engine="openpyxl")
        except Exception as openpyxl_error:
            raise ValueError(
                f"无法读取 Excel 文件 {path.name}: {calamine_error}; fallback: {openpyxl_error}"
            ) from openpyxl_error


def _read_csv(path: Path) -> pl.DataFrame:
    """Read UTF-8 CSV files while preserving identifier columns as text."""
    try:
        return pl.read_csv(path, infer_schema=False, try_parse_dates=False, encoding="utf8")
    except Exception as utf8_error:
        try:
            text = path.read_bytes().decode("gb18030")
            return pl.read_csv(io.StringIO(text), infer_schema=False, try_parse_dates=False)
        except Exception as gb18030_error:
            raise ValueError(
                f"无法读取 CSV 文件 {path.name}: {utf8_error}; fallback: {gb18030_error}"
            ) from gb18030_error


def _read_table(path: Path) -> pl.DataFrame:
    if path.suffix.lower() == ".csv":
        return _read_csv(path)
    if path.suffix.lower() in {".xlsx", ".xls"}:
        return _read_excel(path)
    raise ValueError(f"不支持的文件类型: {path.name}")


def normalise_frame(frame: pl.DataFrame) -> tuple[pl.DataFrame, dict[str, int]]:
    """Keep categorical columns textual, while exposing typed LIS fields for vectorized analysis."""
    names: list[str] = []
    used: set[str] = set()
    for index, name in enumerate(frame.columns):
        clean = str(name).strip() or f"未命名列_{index + 1}"
        if clean in used:
            suffix = 2
            while f"{clean}_{suffix}" in used:
                suffix += 1
            clean = f"{clean}_{suffix}"
        names.append(clean)
        used.add(clean)
    frame = frame.rename(dict(zip(frame.columns, names)))

    invalid_counts: dict[str, int] = {}
    expressions: list[pl.Expr] = []
    for col in frame.columns:
        raw_text = frame.get_column(col).cast(pl.Utf8, strict=False).str.strip_chars()
        invalid_counts[col] = int(raw_text.is_in(list(NA_MARKERS)).sum())
        cleaned = _clean_text_expr(pl.col(col))
        if col in NUMERIC_COLUMNS:
            expressions.append(cleaned.cast(pl.Float64, strict=False).alias(col))
        elif col in DATE_COLUMNS:
            parsed = [_parse_excel_date(v) for v in frame.get_column(col).to_list()]
            expressions.append(pl.Series(col, parsed, dtype=pl.Datetime("us")))
        else:
            expressions.append(cleaned.alias(col))

    # Series expressions are accepted by with_columns alongside Exprs.
    normalised = frame.with_columns(expressions)
    return normalised, invalid_counts


class DatasetStore:
    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._metadata: dict[str, dict[str, Any]] = {}
        self._frames: dict[str, pl.DataFrame] = {}

    def create_from_files(self, paths: Iterable[Path], display_name: str) -> dict[str, Any]:
        files = list(paths)
        if not files:
            raise ValueError("没有可读取的 Excel 或 CSV 文件")
        frames = [_read_table(path) for path in files]
        try:
            combined = pl.concat(frames, how="diagonal_relaxed")
        except Exception as exc:
            raise ValueError(f"多个文件的列结构无法合并: {exc}") from exc
        frame, invalid_counts = normalise_frame(combined)
        dataset_id = uuid.uuid4().hex
        dataset_dir = settings.datasets_dir / dataset_id
        dataset_dir.mkdir(parents=True, exist_ok=False)
        parquet_path = dataset_dir / "data.parquet"
        frame.write_parquet(parquet_path, compression="zstd")
        schema = self._schema(frame)
        metadata = {
            "dataset_id": dataset_id,
            "name": display_name,
            "row_count": frame.height,
            "columns": frame.columns,
            "schema": schema,
            "invalid_counts": invalid_counts,
            "parquet_path": str(parquet_path),
        }
        (dataset_dir / "metadata.json").write_text(
            json.dumps(metadata, ensure_ascii=False, indent=2), encoding="utf-8"
        )
        with self._lock:
            self._metadata[dataset_id] = metadata
            self._frames[dataset_id] = frame
        return metadata

    def _schema(self, frame: pl.DataFrame) -> list[dict[str, Any]]:
        result: list[dict[str, Any]] = []
        for name, dtype in frame.schema.items():
            series = frame.get_column(name)
            unique = int(series.n_unique())
            item: dict[str, Any] = {
                "name": name,
                "dtype": str(dtype),
                "nullable": bool(series.null_count() > 0),
                "unique_count": unique,
            }
            if dtype.is_numeric() or dtype in (pl.Date, pl.Datetime) or str(dtype).startswith("Datetime"):
                item["min"] = _json_value(series.min())
                item["max"] = _json_value(series.max())
            if unique <= 200:
                choices = [_json_value(x) for x in series.drop_nulls().unique().sort().to_list()]
                if series.null_count():
                    choices.append("NA")
                item["choices"] = choices
            result.append(item)
        return result

    def metadata(self, dataset_id: str) -> dict[str, Any]:
        with self._lock:
            if dataset_id in self._metadata:
                return self._metadata[dataset_id]
        path = settings.datasets_dir / dataset_id / "metadata.json"
        if not path.exists():
            raise KeyError(dataset_id)
        metadata = json.loads(path.read_text(encoding="utf-8"))
        with self._lock:
            self._metadata[dataset_id] = metadata
        return metadata

    def frame(self, dataset_id: str) -> pl.DataFrame:
        with self._lock:
            cached = self._frames.get(dataset_id)
            if cached is not None:
                return cached
        metadata = self.metadata(dataset_id)
        path = Path(metadata["parquet_path"])
        if not path.exists():
            raise KeyError(dataset_id)
        frame = pl.read_parquet(path)
        with self._lock:
            self._frames[dataset_id] = frame
        return frame

    def invalidate(self, dataset_id: str) -> None:
        with self._lock:
            self._frames.pop(dataset_id, None)
            self._metadata.pop(dataset_id, None)

    def apply_filter(self, dataset_id: str, request: FilterRequest) -> pl.DataFrame:
        frame = self.frame(dataset_id)
        return apply_filter(frame, request)

    def rows(self, dataset_id: str, request: RowsRequest) -> tuple[list[str], list[dict[str, Any]], int]:
        filtered = apply_filter(self.frame(dataset_id), request)
        if request.sort_by and request.sort_by in filtered.columns:
            filtered = filtered.sort(request.sort_by, descending=request.descending, nulls_last=True)
        total = filtered.height
        page = filtered.slice(request.offset, request.limit)
        return filtered.columns, _json_rows(page), total

    def filter_metadata(self, dataset_id: str) -> list[dict[str, Any]]:
        return self._schema(self.frame(dataset_id))

    def replace_frame(self, dataset_id: str, frame: pl.DataFrame) -> dict[str, Any]:
        metadata = self.metadata(dataset_id)
        path = Path(metadata["parquet_path"])
        frame.write_parquet(path, compression="zstd")
        metadata = {
            **metadata,
            "row_count": frame.height,
            "columns": frame.columns,
            "schema": self._schema(frame),
        }
        (path.parent / "metadata.json").write_text(
            json.dumps(metadata, ensure_ascii=False, indent=2), encoding="utf-8"
        )
        with self._lock:
            self._metadata[dataset_id] = metadata
            self._frames[dataset_id] = frame
        return metadata


def _parse_range(value: Any, dtype: pl.DataType) -> Any:
    if value is None:
        return None
    if dtype in (pl.Date, pl.Datetime) or str(dtype).startswith("Datetime"):
        return _parse_excel_date(value)
    try:
        return float(value)
    except (TypeError, ValueError):
        return value


def apply_filter(frame: pl.DataFrame, request: FilterRequest) -> pl.DataFrame:
    result = frame
    for col, values in request.categorical.items():
        if col in result.columns and values:
            include_na = "NA" in values
            normal_values = [value for value in values if value != "NA"]
            expression = pl.col(col).cast(pl.Utf8, strict=False).is_in(normal_values)
            if include_na:
                expression = expression | pl.col(col).is_null()
            result = result.filter(expression)
    for col, bounds in request.ranges.items():
        if col not in result.columns or not bounds or len(bounds) < 2:
            continue
        low, high = bounds[0], bounds[1]
        dtype = result.schema[col]
        if low is not None:
            result = result.filter(pl.col(col) >= _parse_range(low, dtype))
        if high is not None:
            result = result.filter(pl.col(col) <= _parse_range(high, dtype))

    # These switches mirror the Shiny dashboard's three invalid-value controls.
    if request.invalid_date and "采样时间" in result.columns:
        result = result.filter(pl.col("采样时间").is_not_null())
    if request.invalid_age and "年龄" in result.columns:
        result = result.filter(pl.col("年龄").is_not_null())
    if request.invalid_result and "定量结果" in result.columns:
        result = result.filter(pl.col("定量结果").is_not_null())
    return result


def frame_fingerprint(frame: pl.DataFrame, columns: Iterable[str]) -> str:
    selected = frame.select([c for c in columns if c in frame.columns])
    payload = selected.write_ipc(compression="zstd")
    return hashlib.sha256(payload).hexdigest()


dataset_store = DatasetStore()
