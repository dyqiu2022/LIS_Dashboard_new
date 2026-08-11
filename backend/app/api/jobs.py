from __future__ import annotations

from fastapi import APIRouter, HTTPException

from app.services.jobs import job_store

router = APIRouter(prefix="/api/jobs", tags=["jobs"])


@router.get("/{job_id}")
def get_job(job_id: str):
    try:
        return job_store.get(job_id)
    except KeyError as exc:
        raise HTTPException(404, "任务不存在") from exc
