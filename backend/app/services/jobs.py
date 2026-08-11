from __future__ import annotations

import json
import threading
import uuid
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

from app.config import settings


class JobStore:
    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._jobs: dict[str, dict[str, Any]] = {}

    def _path(self, job_id: str) -> Path:
        return settings.jobs_dir / f"{job_id}.json"

    def create(self, kind: str) -> dict[str, Any]:
        job = {
            "job_id": uuid.uuid4().hex,
            "kind": kind,
            "status": "queued",
            "progress": 0.0,
            "detail": "等待计算",
            "result": None,
            "error": None,
            "created_at": datetime.now(timezone.utc).isoformat(),
        }
        self.update(job["job_id"], **{key: value for key, value in job.items() if key != "job_id"})
        return job

    def get(self, job_id: str) -> dict[str, Any]:
        path = self._path(job_id)
        if path.exists():
            job = json.loads(path.read_text(encoding="utf-8"))
            with self._lock:
                self._jobs[job_id] = job
            return job
        with self._lock:
            if job_id in self._jobs:
                return dict(self._jobs[job_id])
        raise KeyError(job_id)

    def artifact_path(self, job_id: str) -> Path:
        return settings.jobs_dir / f"{job_id}.artifact.json"

    def save_artifact(self, job_id: str, artifact: Any) -> None:
        path = self.artifact_path(job_id)
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
        temporary.write_text(json.dumps(artifact, ensure_ascii=False, default=str), encoding="utf-8")
        temporary.replace(path)

    def get_artifact(self, job_id: str) -> Any:
        path = self.artifact_path(job_id)
        if not path.exists():
            return None
        return json.loads(path.read_text(encoding="utf-8"))

    def update(self, job_id: str, **values: Any) -> dict[str, Any]:
        with self._lock:
            job = dict(self._jobs.get(job_id, {
                "job_id": job_id,
                "kind": values.get("kind", "unknown"),
                "status": "queued",
                "progress": 0.0,
                "detail": "等待计算",
                "result": None,
                "error": None,
            }))
            job.update(values)
            job.setdefault("job_id", job_id)
            self._jobs[job_id] = job
            path = self._path(job_id)
            path.parent.mkdir(parents=True, exist_ok=True)
            temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
            temporary.write_text(json.dumps(job, ensure_ascii=False, default=str), encoding="utf-8")
            temporary.replace(path)
            return dict(job)

    def progress(self, job_id: str, value: float, detail: str = "") -> None:
        self.update(job_id, status="running", progress=max(0.0, min(1.0, value)), detail=detail)


job_store = JobStore()
# Embedding jobs share one GPU model; queue them instead of risking two model
# forwards and an out-of-memory failure on the 16 GB test card.
_executor = ThreadPoolExecutor(max_workers=1, thread_name_prefix="lis-job")


def _run(job_id: str, kind: str, fn: Callable[..., Any], args: tuple[Any, ...], kwargs: dict[str, Any]) -> None:
    job_store.update(job_id, status="running", detail="开始计算")
    try:
        result = fn(*args, progress_callback=lambda value, detail="": job_store.progress(job_id, value, detail), **kwargs)
        if isinstance(result, dict) and "_artifact" in result:
            artifact = result.pop("_artifact")
            job_store.save_artifact(job_id, artifact)
            result["artifact_available"] = True
        job_store.update(job_id, status="completed", progress=1.0, detail="计算完成", result=result)
    except Exception as exc:  # surfaced through the status endpoint instead of killing API workers
        job_store.update(job_id, status="failed", detail="计算失败", error=f"{type(exc).__name__}: {exc}")


def submit(kind: str, fn: Callable[..., Any], *args: Any, payload: dict[str, Any] | None = None,
           **kwargs: Any) -> dict[str, Any]:
    job = job_store.create(kind)
    if not settings.local_jobs and settings.redis_url and payload is not None:
        try:
            from app.celery_app import execute_job
            if execute_job is not None:
                execute_job.delay(job["job_id"], kind, payload)
                return job
        except Exception as exc:
            job_store.update(job["job_id"], detail=f"分布式队列不可用，回退本地任务: {exc}")
    _executor.submit(_run, job["job_id"], kind, fn, args, kwargs)
    return job
