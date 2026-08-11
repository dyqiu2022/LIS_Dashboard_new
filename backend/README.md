# LIS Dashboard FastAPI backend

本目录是 R Shiny 的 Python 重写后端。运行时数据、任务结果和向量缓存均写入 `LIS_DATA_DIR`，模型权重写入 `LIS_MODEL_DIR`，不进入 Git。

```bash
pip install -e '.[test]'
uvicorn app.main:app --reload
```

Docker 默认构建 CPU-only PyTorch，避免把 CUDA 运行库塞进普通部署镜像；GPU 环境使用 `Dockerfile.gpu` 和支持 GPU 的 Docker Engine。默认词嵌入模型为支持指令提示词的 `codefuse-ai/F2LLM-v2-4B`，同时可在聚类界面选择 `F2LLM-v2-1.7B`；权重只下载到宿主机模型挂载目录，不会进入项目目录或镜像层。生产环境可用 `LIS_PREWARM_MODELS` 一次预热两个模型，再使用 `LIS_MODEL_LOCAL_ONLY=1` 启动 API/worker；可通过 `LIS_EMBEDDING_MODEL`、`LIS_EMBEDDING_DIM`、`LIS_EMBEDDING_BATCH_SIZE`、`LIS_EMBEDDING_DEVICE` 和 `LIS_MODEL_DIR` 调整。
