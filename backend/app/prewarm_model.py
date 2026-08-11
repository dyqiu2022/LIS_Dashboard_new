"""Download and load the pinned embedding model before serving traffic.

Weights are written directly to the persistent host-mounted model directory,
not to the project tree or an opaque Docker volume. The final smoke test also
checks the custom instruction format used by F2LLM-v2.
"""

import os

from huggingface_hub import snapshot_download

from app.config import settings
from app.services.embedding import embedding_service


if __name__ == "__main__":
    model_names = [
        name.strip()
        for name in os.getenv("LIS_PREWARM_MODELS", settings.model_name).split(",")
        if name.strip()
    ]
    for model_name in model_names:
        model_path = settings.model_dir / model_name.replace("/", "--")
        snapshot = snapshot_download(
            repo_id=model_name,
            local_dir=str(model_path),
            local_files_only=False,
            token=settings.hf_token or None,
        )
        if not embedding_service._model_is_complete(model_path):
            raise RuntimeError(f"模型下载不完整，缺少必要文件: {model_path}")
        vectors = embedding_service.encode(
            ["胃功能异常", "其他腹腔疾病"],
            model_name=model_name,
            dimension=settings.embedding_dim,
            batch_size=2,
            instruction=settings.embedding_instruction,
        )
        print(
            f"model ready: {model_name}, snapshot={snapshot}, "
            f"shape={vectors.shape}, device={settings.embedding_device}, dtype={settings.embedding_dtype}"
        )
