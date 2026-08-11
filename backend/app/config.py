from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path


DEFAULT_EMBEDDING_INSTRUCTION = "对临床诊断进行分类，生成适合聚类且有代表性的词向量"


@dataclass(frozen=True)
class Settings:
    app_name: str = "LIS 数据看板 API"
    host: str = os.getenv("LIS_HOST", "0.0.0.0")
    port: int = int(os.getenv("LIS_PORT", "8000"))
    data_dir: Path = Path(os.getenv("LIS_DATA_DIR", "./.lis-data")).resolve()
    model_dir: Path = Path(os.getenv("LIS_MODEL_DIR", "./.lis-models")).resolve()
    model_name: str = os.getenv("LIS_EMBEDDING_MODEL", "codefuse-ai/F2LLM-v2-4B")
    embedding_dim: int = int(os.getenv("LIS_EMBEDDING_DIM", "1024"))
    embedding_batch_size: int = int(os.getenv("LIS_EMBEDDING_BATCH_SIZE", "32"))
    embedding_device: str = os.getenv("LIS_EMBEDDING_DEVICE", "auto")
    embedding_dtype: str = os.getenv("LIS_EMBEDDING_DTYPE", "auto")
    embedding_instruction: str = os.getenv("LIS_EMBEDDING_INSTRUCTION", DEFAULT_EMBEDDING_INSTRUCTION)
    model_local_only: bool = os.getenv("LIS_MODEL_LOCAL_ONLY", "0").lower() not in {"0", "false", "no"}
    hf_home: Path = Path(os.getenv("HF_HOME", str(model_dir / "huggingface"))).resolve()
    hf_cache_dir: Path = Path(os.getenv("HUGGINGFACE_HUB_CACHE", str(hf_home / "hub"))).resolve()
    hf_token: str = os.getenv("HF_TOKEN", "")
    max_upload_mb: int = int(os.getenv("LIS_MAX_UPLOAD_MB", "1000"))
    local_jobs: bool = os.getenv("LIS_LOCAL_JOBS", "1").lower() not in {"0", "false", "no"}
    redis_url: str = os.getenv("LIS_REDIS_URL", "")
    allow_origins: tuple[str, ...] = tuple(
        x.strip() for x in os.getenv("LIS_ALLOW_ORIGINS", "*").split(",") if x.strip()
    )

    @property
    def datasets_dir(self) -> Path:
        return self.data_dir / "datasets"

    @property
    def jobs_dir(self) -> Path:
        return self.data_dir / "jobs"

    @property
    def cache_dir(self) -> Path:
        return self.data_dir / "cache"

    def ensure_dirs(self) -> None:
        for path in (self.data_dir, self.datasets_dir, self.jobs_dir, self.cache_dir,
                     self.model_dir, self.hf_home, self.hf_cache_dir):
            path.mkdir(parents=True, exist_ok=True)


settings = Settings()
settings.ensure_dirs()
