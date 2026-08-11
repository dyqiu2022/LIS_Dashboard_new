from __future__ import annotations

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from app.api import analysis, batch, clustering, datasets, jobs
from app.config import settings
from app.services.data_store import dataset_store

app = FastAPI(
    title=settings.app_name,
    version="0.1.0",
)
app.add_middleware(
    CORSMiddleware,
    allow_origins=list(settings.allow_origins),
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
app.include_router(datasets.router)
app.include_router(analysis.router)
app.include_router(clustering.router)
app.include_router(batch.router)
app.include_router(jobs.router)


@app.get("/healthz", tags=["system"])
def healthz():
    return {"status": "ok", "model": settings.model_name, "embedding_dim": settings.embedding_dim}


@app.get("/api", tags=["system"])
@app.get("/api/", include_in_schema=False, tags=["system"])
def api_root():
    return {"status": "ok", "service": settings.app_name, "docs": "/docs", "health": "/healthz"}


@app.get("/api/columns/{dataset_id}", tags=["datasets"])
def columns(dataset_id: str):
    from fastapi import HTTPException

    try:
        metadata = dataset_store.metadata(dataset_id)
    except KeyError as exc:
        raise HTTPException(404, "数据集不存在") from exc
    return {"columns": metadata["columns"], "schema": metadata["schema"]}
