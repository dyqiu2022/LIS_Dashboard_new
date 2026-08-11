from pathlib import Path
from types import SimpleNamespace

import numpy as np
import polars as pl

from app.services.batch_difference import build_point_detail, run_batch_difference
from app.services.colors import string_to_color
from app.services.embedding import EmbeddingService
from app.services.clustering import _nearest_phrases
from app.services.data_store import DatasetStore, _read_csv, normalise_frame
from app.services.discretize import discretize_age, parse_cut_points
from app.services.keyword import KeywordDefinition, matches_text
from app.services.qualitative import discrete_stack, pie
from app.services.quantitative import boxcox_fit, boxcox_inverse, histogram, trend


SAMPLE = Path(__file__).resolve().parents[2] / "深圳市龙岗区人民医院 CA125(已清洗).xlsx"


def test_normalise_preserves_categorical_values_and_types():
    frame = pl.DataFrame({"年龄": ["20", "NA"], "定量结果": ["1.2", "结果无效"], "性别": ["女", "男"]})
    clean, invalid = normalise_frame(frame)
    assert clean.schema["年龄"] == pl.Float64
    assert clean.schema["定量结果"] == pl.Float64
    assert clean["性别"].to_list() == ["女", "男"]
    assert clean["年龄"].to_list() == [20.0, None]
    assert invalid["年龄"] == 1


def test_csv_reader_preserves_identifiers_and_normalises_lis_fields(tmp_path):
    path = tmp_path / "lis_data.csv"
    path.write_text(
        "医院名称,病人id,采样时间,年龄,定量结果\n"
        "测试医院,00910107,2025-08-25 10:05:00,67,12.97\n",
        encoding="utf-8",
    )
    clean, _ = normalise_frame(_read_csv(path))
    assert clean["病人id"].to_list() == ["00910107"]
    assert clean.schema["年龄"] == pl.Float64
    assert clean.schema["定量结果"] == pl.Float64
    assert str(clean["采样时间"].dtype).startswith("Datetime")


def test_store_reads_sample_and_dynamic_filter(tmp_path, monkeypatch):
    if not SAMPLE.exists():
        return
    monkeypatch.setenv("LIS_DATA_DIR", str(tmp_path))
    store = DatasetStore()
    metadata = store.create_from_files([SAMPLE], "sample")
    assert metadata["row_count"] == 72194
    assert "临床诊断" in metadata["columns"]
    from app.schemas import RowsRequest

    request = RowsRequest(categorical={"性别": ["女"]}, ranges={"年龄": [40, 60]}, limit=10)
    _, rows, total = store.rows(metadata["dataset_id"], request)
    assert total > 0
    assert len(rows) <= 10


def test_nearest_phrases_handles_short_chunks():
    embeddings = np.eye(2, dtype=np.float32)
    centers = np.eye(2, dtype=np.float32)
    result = _nearest_phrases(embeddings, centers, ["甲", "乙"], top_n=3)
    assert result == ["甲、乙", "乙、甲"]


def test_embedding_cache_uses_compact_sqlite_store(tmp_path, monkeypatch):
    monkeypatch.setattr("app.services.embedding.settings", SimpleNamespace(
        cache_dir=tmp_path, model_dir=tmp_path, embedding_dim=4,
        embedding_batch_size=2, embedding_device="cpu", model_name="test",
    ))
    service = EmbeddingService()
    key = service._key("test", 4, "短语")
    service._write_cache({key: np.array([1, 0, 0, 0], dtype=np.float32)}, 4)
    result = service.encode(["短语", "短语"], model_name="test", dimension=4)
    assert result.shape == (2, 4)
    assert (tmp_path / "embeddings.sqlite3").exists()


def test_keyword_semantics():
    definition = KeywordDefinition(class_name="糖尿病", and1="糖尿病|DM", not_words="妊娠", not_limit="既往")
    assert matches_text("2型糖尿病", definition)
    assert not matches_text("妊娠期糖尿病", definition)
    assert matches_text("既往妊娠糖尿病", definition)


def test_discretization_and_boxcox_round_trip():
    values = np.array([1, 2, 3, 4, 5, 6], dtype=float)
    info = parse_cut_points("30%|70%", values)
    assert info.pattern == "%"
    groups, labels = discretize_age(values, 2)
    assert len(labels) == 4
    transformed, lam = boxcox_fit(values)
    restored = boxcox_inverse(transformed, lam)
    assert np.allclose(restored, values, atol=1e-6)


def test_chart_services_return_aggregated_specs():
    frame = pl.DataFrame({"性别": ["男", "女", "女"], "定性结果": ["正常", "正常", "偏高"], "年龄": [20, 30, 30], "定量结果": [1.0, 2.0, 3.0]})
    assert pie(frame, "性别")["data"]
    assert discrete_stack(frame, "性别", "定性结果", 2, "数量", "默认颜色")["data"]
    assert trend(frame, "性别", ["50%"], 1, 1)["data"]
    assert string_to_color("测试").startswith("#")


def test_legacy_analysis_reports_and_plot_details():
    frame = pl.DataFrame({
        "采样时间": [f"2024-01-{i:02d}" for i in range(1, 13)],
        "定量结果": [1.0 + i * 0.1 for i in range(12)],
        "年龄": [20 + i % 5 for i in range(12)],
        "性别": ["男", "女"] * 6,
    })
    clean, _ = normalise_frame(frame)
    report = histogram(clean, "定量结果", "5%|30%|50%|70%|95%", "Box-Cox", 8)
    assert len(report["distribution_table"]) == 21
    assert report["discretized_plot"]["data"]
    spec = trend(clean, "性别", ["95%", "50%", "5%"], 5, 1)
    assert any("error_y" in trace for trace in spec["data"])
    assert any(trace.get("mode") == "markers" for trace in spec["data"])


def test_batch_difference_small_frame():
    frame = pl.DataFrame({
        "采样时间": [f"2024-01-{i:02d}" for i in range(1, 13)],
        "定量结果": [1.0 + i * 0.1 for i in range(12)],
        "年龄": [20 + i for i in range(12)],
        "性别": ["男", "女"] * 6,
        "试剂厂家": ["A"] * 12,
    })
    clean, _ = normalise_frame(frame)
    result = run_batch_difference(clean, "性别:I(年龄^2)", 5, 2)
    assert result["all_manu_data"]
    assert result["stats_summary"]
    assert "_artifact" in result
    assert {"point_id", "等效水平", "p_value_bonferroni", "original_indices"}.issubset(result["all_manu_data"][0])
    artifact = result.pop("_artifact")
    detail = build_point_detail(result["all_manu_data"][0]["point_id"], result, artifact, [])
    assert detail["density"]["data"] or detail["density"]["layout"]["title"]
