from __future__ import annotations

from typing import Any

import numpy as np
import polars as pl

from app.services.embedding import embedding_service


def _nearest_phrases(embeddings: np.ndarray, centers: np.ndarray, phrases: list[str], top_n: int = 3) -> list[str]:
    normalized_centers = centers / np.maximum(np.linalg.norm(centers, axis=1, keepdims=True), 1e-12)
    try:
        import faiss  # type: ignore

        index = faiss.IndexFlatIP(embeddings.shape[1])
        index.add(embeddings.astype(np.float32))
        _, ids = index.search(normalized_centers.astype(np.float32), top_n)
        return ["、".join(phrases[i] for i in row if i >= 0) for row in ids]
    except ImportError:
        # Bounded chunks avoid allocating a vocabulary-by-cluster matrix.
        output: list[list[tuple[float, str]]] = [[] for _ in range(len(centers))]
        for start in range(0, len(embeddings), 8192):
            chunk = embeddings[start:start + 8192]
            scores = chunk @ normalized_centers.T
            for center_index in range(scores.shape[1]):
                take = min(top_n, len(chunk))
                ids = np.argpartition(scores[:, center_index], -take)[-take:]
                output[center_index].extend((float(scores[i, center_index]), phrases[start + i]) for i in ids)
        return ["、".join(x[1] for x in sorted(items, reverse=True)[:top_n]) for items in output]


def run_unsupervised(frame: pl.DataFrame, grouping_col: str, cluster_num: int,
                     model_name: str | None = None, embedding_dim: int | None = None,
                     instruction: str | None = None, progress_callback=None) -> dict[str, Any]:
    if progress_callback:
        progress_callback(0.02, "正在汇总聚类短语")
    if grouping_col not in frame.columns:
        raise ValueError(f"不存在聚类列: {grouping_col}")
    grouped = (
        frame.select([grouping_col])
        .with_columns(pl.col(grouping_col).cast(pl.Utf8).fill_null("NA"))
        .group_by(grouping_col)
        .len(name="数量")
        .sort("数量", descending=True)
    )
    phrases = [str(x) for x in grouped[grouping_col].to_list()]
    counts = grouped["数量"].to_numpy().astype(np.float32)
    if len(phrases) < 2:
        raise ValueError("有效短语不足，无法聚类")
    k = min(max(2, int(cluster_num)), len(phrases))
    if progress_callback:
        progress_callback(0.05, f"准备对 {len(phrases)} 个短语进行向量化")

    def embedding_progress(value: float, detail: str = "") -> None:
        if progress_callback:
            progress_callback(0.05 + min(max(value, 0.0), 1.0) * 0.65, detail)

    embeddings = embedding_service.encode(phrases, model_name=model_name, dimension=embedding_dim,
                                          instruction=instruction, progress_callback=embedding_progress)
    if progress_callback:
        progress_callback(0.74, "向量化完成，正在训练聚类模型")
    try:
        from sklearn.cluster import MiniBatchKMeans
    except ImportError as exc:
        raise RuntimeError("未安装 scikit-learn") from exc
    model = MiniBatchKMeans(
        n_clusters=k,
        random_state=123,
        n_init=10,
        batch_size=min(4096, max(256, len(phrases))),
        max_iter=100,
        reassignment_ratio=0.01,
    )
    try:
        model.fit(embeddings, sample_weight=counts)
    except TypeError:
        # Compatibility fallback for old sklearn releases.
        model.fit(np.repeat(embeddings, np.maximum(1, np.ceil(counts / counts.max() * 10).astype(int)), axis=0))
    if progress_callback:
        progress_callback(0.9, "正在整理聚类结果")
    labels = model.labels_.astype(int) + 1
    centers = np.asarray(model.cluster_centers_, dtype=np.float32)
    nearest = _nearest_phrases(embeddings, centers, phrases)
    rows: list[dict[str, Any]] = []
    for cluster in range(1, k + 1):
        mask = labels == cluster
        sample_count = float(counts[mask].sum())
        distances = np.linalg.norm(embeddings[mask] - centers[cluster - 1], axis=1) if mask.any() else np.array([])
        for phrase, count, label, distance in zip(
            np.asarray(phrases)[mask], counts[mask], labels[mask], distances
        ):
            rows.append({
                "类综合排名": cluster,
                "类别_无监督": nearest[cluster - 1],
                "类别数量": sample_count,
                grouping_col: phrase,
                f"{grouping_col}数量": int(count),
                "分词与词性": f"{phrase}（句向量）",
                "类紧密度": float(distance),
            })
    rows.sort(key=lambda row: (row["类综合排名"], row["类紧密度"]))
    if progress_callback:
        progress_callback(0.97, f"已整理 {len(rows)} 条聚类结果")
    return {"rows": rows, "cluster_num": k, "embedding_dim": embeddings.shape[1],
            "phrase_count": len(phrases), "center_phrases": nearest}
