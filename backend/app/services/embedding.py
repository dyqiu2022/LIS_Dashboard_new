from __future__ import annotations

import hashlib
import json
import sqlite3
import threading
from pathlib import Path
from typing import Iterable

import numpy as np

from app.config import settings


class EmbeddingService:
    """Process-local model singleton with an on-disk phrase cache.

    The service intentionally loads the model lazily. The API can therefore start and serve
    data-analysis endpoints on CPU-only machines even when model weights are not installed yet.
    """

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._model = None
        self._model_name: str | None = None
        self._dimension: int | None = None
        self._cache_lock = threading.RLock()
        self._encode_lock = threading.Lock()

    @staticmethod
    def _model_path(selected: str) -> Path:
        return settings.model_dir / selected.replace("/", "--")

    @staticmethod
    def _model_is_complete(model_path: Path) -> bool:
        """Require a real SentenceTransformer snapshot, not merely a partial directory."""
        required = (
            model_path / "config.json",
            model_path / "modules.json",
            model_path / "tokenizer_config.json",
            model_path / "1_Pooling" / "config.json",
        )
        weights = list(model_path.glob("*.safetensors")) + list(model_path.glob("*.bin"))
        return model_path.is_dir() and all(path.is_file() for path in required) and bool(weights)

    def _resolve_model_source(self, selected: str) -> str:
        """Resolve/download a model into the persistent model mount, never the project tree."""
        model_path = self._model_path(selected)
        local_only = getattr(settings, "model_local_only", False)
        if self._model_is_complete(model_path):
            return str(model_path)
        if local_only and model_path.exists():
            raise RuntimeError(f"本地词嵌入模型文件不完整: {model_path}")

        try:
            from huggingface_hub import snapshot_download
        except ImportError as exc:
            raise RuntimeError("未安装 huggingface_hub，无法定位本地词嵌入模型") from exc

        try:
            return snapshot_download(
                repo_id=selected,
                local_dir=str(model_path),
                local_files_only=local_only,
                token=getattr(settings, "hf_token", "") or None,
            )
        except Exception as exc:
            if local_only:
                raise RuntimeError(
                    f"本地词嵌入模型不存在: {selected}。请先执行 `python -m app.prewarm_model` 下载到模型挂载目录。"
                ) from exc
            raise

    def _load(self, model_name: str | None = None):
        selected = model_name or settings.model_name
        with self._lock:
            if self._model is not None and self._model_name == selected:
                return self._model
            if self._model is not None:
                # A user can switch between 4B and 1.7B in the UI. Release the
                # previous model before loading the next one, otherwise a 16 GB
                # test GPU can retain both weights and run out of VRAM.
                import gc
                old_model = self._model
                self._model = None
                self._model_name = None
                del old_model
                gc.collect()
                try:
                    import torch
                    if torch.cuda.is_available():
                        torch.cuda.empty_cache()
                except ImportError:
                    pass
            try:
                from sentence_transformers import SentenceTransformer
            except ImportError as exc:
                raise RuntimeError("未安装 sentence-transformers，请安装 backend 依赖") from exc
            source = self._resolve_model_source(selected)
            device = settings.embedding_device
            if device == "auto":
                try:
                    import torch
                    device = "cuda" if torch.cuda.is_available() else "cpu"
                except ImportError:
                    device = "cpu"
            kwargs = {"trust_remote_code": True, "device": device}
            dtype = str(getattr(settings, "embedding_dtype", "auto")).lower()
            if dtype == "auto":
                dtype = "bfloat16" if str(device).startswith("cuda") else ""
            if dtype and dtype not in {"float32", "float16", "bfloat16"}:
                raise ValueError("LIS_EMBEDDING_DTYPE 必须是 auto、float32、float16 或 bfloat16")
            if dtype:
                kwargs["model_kwargs"] = {"torch_dtype": dtype}
            try:
                self._model = SentenceTransformer(source, **kwargs)
            except TypeError:
                # Older sentence-transformers versions may not expose one of these options.
                kwargs.pop("trust_remote_code", None)
                try:
                    self._model = SentenceTransformer(source, **kwargs)
                except TypeError:
                    kwargs.pop("model_kwargs", None)
                    self._model = SentenceTransformer(source, **kwargs)
            self._model_name = selected
            return self._model

    @staticmethod
    def _key(model_name: str, dimension: int, text: str, instruction: str | None = None) -> str:
        cleaned = (instruction or "").strip()
        payload = [model_name, dimension, text] if not cleaned else [model_name, dimension, cleaned, text]
        raw = json.dumps(payload, ensure_ascii=False, separators=(",", ":"))
        return hashlib.sha256(raw.encode("utf-8")).hexdigest()

    @staticmethod
    def _format_instruction(model_name: str, instruction: str | None, text: str) -> str:
        cleaned = (instruction or "").strip()
        if not cleaned:
            return text
        if "f2llm" in model_name.lower():
            return f"Instruct: {cleaned}\nQuery: {text}"
        return f"{cleaned}\n{text}"

    def _cache_path(self, key: str) -> Path:
        # Kept as a read-only migration path for caches created by the first prototype.
        return settings.cache_dir / "embeddings" / key[:2] / f"{key}.npy"

    def _cache_connection(self) -> sqlite3.Connection:
        path = settings.cache_dir / "embeddings.sqlite3"
        connection = sqlite3.connect(path, timeout=60)
        connection.execute("PRAGMA journal_mode=WAL")
        connection.execute("PRAGMA synchronous=NORMAL")
        connection.execute("CREATE TABLE IF NOT EXISTS vectors (key TEXT PRIMARY KEY, dimension INTEGER NOT NULL, vector BLOB NOT NULL)")
        return connection

    def _read_cache(self, keys: list[str], dimension: int) -> dict[str, np.ndarray]:
        if not keys:
            return {}
        found: dict[str, np.ndarray] = {}
        with self._cache_lock, self._cache_connection() as connection:
            for start in range(0, len(keys), 500):
                chunk = keys[start:start + 500]
                placeholders = ",".join("?" for _ in chunk)
                query = f"SELECT key, vector FROM vectors WHERE dimension = ? AND key IN ({placeholders})"
                for key, blob in connection.execute(query, [dimension, *chunk]):
                    found[key] = np.frombuffer(blob, dtype=np.float16).astype(np.float32)
        return found

    def _write_cache(self, vectors: dict[str, np.ndarray], dimension: int) -> None:
        if not vectors:
            return
        with self._cache_lock, self._cache_connection() as connection:
            connection.executemany(
                "INSERT OR REPLACE INTO vectors(key, dimension, vector) VALUES (?, ?, ?)",
                [(key, dimension, sqlite3.Binary(vector.astype(np.float16).tobytes()))
                 for key, vector in vectors.items()],
            )

    def encode(self, texts: Iterable[str], model_name: str | None = None,
               dimension: int | None = None, batch_size: int | None = None,
               instruction: str | None = None, progress_callback=None) -> np.ndarray:
        values = ["" if text is None else str(text) for text in texts]
        if not values:
            return np.empty((0, dimension or settings.embedding_dim), dtype=np.float32)
        selected = model_name or settings.model_name
        dim = int(dimension or settings.embedding_dim)
        instruction_text = (instruction or "").strip() or None
        settings.cache_dir.mkdir(parents=True, exist_ok=True)
        result: list[np.ndarray | None] = [None] * len(values)
        texts_by_key: dict[str, str] = {}
        for text in dict.fromkeys(values):
            texts_by_key[self._key(selected, dim, text, instruction_text)] = text
        cached = self._read_cache(list(texts_by_key), dim)
        legacy_hits: dict[str, np.ndarray] = {}
        missing: dict[str, list[int]] = {}
        for index, text in enumerate(values):
            key = self._key(selected, dim, text, instruction_text)
            if key in cached and cached[key].shape == (dim,):
                result[index] = cached[key]
                continue
            # Legacy per-file cache is accepted once, then copied into SQLite below.
            try:
                legacy = np.load(self._cache_path(key), allow_pickle=False)
                if legacy.shape == (dim,):
                    result[index] = legacy.astype(np.float32, copy=False)
                    cached[key] = result[index]
                    legacy_hits[key] = result[index]
                    continue
            except (FileNotFoundError, ValueError, OSError):
                pass
            missing.setdefault(text, []).append(index)

        if progress_callback:
            progress_callback(0.05, f"缓存命中 {len(values) - sum(len(x) for x in missing.values())} / {len(values)} 条")
        if missing:
            model = self._load(selected)
            unique_texts = list(missing)
            model_inputs = [self._format_instruction(selected, instruction_text, text) for text in unique_texts]
            encode_kwargs = {
                "batch_size": int(batch_size or settings.embedding_batch_size),
                "show_progress_bar": False,
                "normalize_embeddings": True,
                "convert_to_numpy": True,
            }
            # F2LLM-v2 supports Matryoshka truncation; keep the persisted vector size
            # stable for the existing clustering/cache schema.
            encode_kwargs["truncate_dim"] = dim
            # A single GPU model is shared by all jobs in this process. Serialize the
            # actual forward pass so a local fallback or API thread cannot duplicate
            # activation memory on a 16 GB card.
            with self._encode_lock:
                try:
                    generated = model.encode(model_inputs, **encode_kwargs)
                except TypeError:
                    encode_kwargs.pop("truncate_dim", None)
                    generated = model.encode(model_inputs, **encode_kwargs)
                    generated = np.asarray(generated)[:, :dim]
            generated = np.asarray(generated, dtype=np.float32)
            if generated.ndim != 2 or generated.shape[1] != dim:
                raise RuntimeError(f"模型输出维度异常: 期望 {dim}，实际 {getattr(generated, 'shape', None)}")
            if progress_callback:
                progress_callback(0.75, f"已完成 {len(unique_texts)} 条短语向量化")
            norms = np.linalg.norm(generated, axis=1, keepdims=True)
            generated = generated / np.maximum(norms, 1e-12)
            generated_cache: dict[str, np.ndarray] = {}
            for text, vector in zip(unique_texts, generated):
                key = self._key(selected, dim, text, instruction_text)
                generated_cache[key] = vector
                for index in missing[text]:
                    result[index] = vector
            self._write_cache(generated_cache, dim)

        # Persist legacy hits in the compact SQLite cache too, so future runs never scan
        # hundreds of thousands of small files.
        if legacy_hits:
            self._write_cache(legacy_hits, dim)
        return np.vstack([vector for vector in result if vector is not None]).astype(np.float32, copy=False)


embedding_service = EmbeddingService()
