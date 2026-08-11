from __future__ import annotations

from typing import Any

from pydantic import BaseModel, ConfigDict, Field

from app.config import settings


class DatasetInfo(BaseModel):
    model_config = ConfigDict(extra="forbid", populate_by_name=True)

    dataset_id: str
    name: str
    row_count: int
    columns: list[str]
    schema_: list[dict[str, Any]] = Field(alias="schema")
    invalid_counts: dict[str, int] = Field(default_factory=dict)


class FilterRequest(BaseModel):
    model_config = ConfigDict(extra="allow")

    invalid_date: bool = True
    invalid_age: bool = True
    invalid_result: bool = True
    categorical: dict[str, list[str]] = Field(default_factory=dict)
    ranges: dict[str, list[float | str | None]] = Field(default_factory=dict)


class RowsRequest(FilterRequest):
    offset: int = Field(default=0, ge=0)
    limit: int = Field(default=100, ge=1, le=5000)
    sort_by: str | None = None
    descending: bool = False


class RowsResponse(BaseModel):
    columns: list[str]
    rows: list[dict[str, Any]]
    total: int
    offset: int
    limit: int


class KeywordDefinition(BaseModel):
    class_name: str = Field(min_length=1, max_length=100)
    and1: str = ""
    and2: str = ""
    and3: str = ""
    not_words: str = ""
    not_limit: str = ""
    exclude_groups: list[str] = Field(default_factory=list)


class KeywordPreviewRequest(FilterRequest):
    grouping_col: str
    definition: KeywordDefinition


class KeywordWriteRequest(FilterRequest):
    grouping_col: str
    definitions: list[KeywordDefinition]


class UnsupervisedRequest(FilterRequest):
    grouping_col: str
    cluster_num: int = Field(default=30, ge=2, le=500)
    embedding_dim: int | None = Field(default=None, ge=32, le=1024)
    model_name: str | None = None
    instruction: str | None = Field(default=settings.embedding_instruction, max_length=2000)


class DiscretizeRequest(FilterRequest):
    column: str
    cut_points: str = "5%|30%|50%|70%|95%"
    transform: str = "原数据"
    bins: int = Field(default=50, ge=1, le=999999)


class QualitativeRequest(FilterRequest):
    primary_col: str
    secondary_col: str | None = None
    color_mode: str = "默认颜色"
    elements_num: int = Field(default=20, ge=0, le=1000)
    y_mode: str = "数量"
    x_var: str = "年龄"
    grain: str | float = "3"
    normalize_quantitative: bool = False
    order_direction: int = 1
    top_x_var: str = "采样时间"
    top_y_mode: str = "数量"
    top_grain: str | float = "month"
    top_normalize_quantitative: bool = False
    bottom_x_var: str = "年龄"
    bottom_y_mode: str = "数量"
    bottom_grain: str | float = "3"
    bottom_normalize_quantitative: bool = False


class QuantitativeRequest(FilterRequest):
    grouping_col: str
    ci: list[str] = Field(default_factory=lambda: ["95%", "80%", "50%", "20%", "5%"])
    hover_mode: str = "x unified"
    smoothing: float = Field(default=1.3, ge=0, le=1.3)
    win_width: int = Field(default=5, ge=1, le=29)
    min_num: int = Field(default=20, ge=1, le=100)


class BatchDifferenceRequest(FilterRequest):
    formula: str = "性别:I(年龄^2) + 性别:年龄 + 类别_无监督"
    n_value: int = Field(default=100, ge=10, le=1000)
    step_value: int = Field(default=100, ge=10, le=1000)


class JobResponse(BaseModel):
    job_id: str
    kind: str
    status: str


class JobStatus(BaseModel):
    job_id: str
    kind: str
    status: str
    progress: float = 0
    detail: str = ""
    result: Any = None
    error: str | None = None
