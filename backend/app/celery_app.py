from __future__ import annotations

from app.config import settings
from app.services.jobs import job_store

try:
    from celery import Celery
except ImportError:  # pragma: no cover - only reached in minimal local installs
    Celery = None  # type: ignore


if Celery is not None:
    celery_app = Celery("lis_dashboard", broker=settings.redis_url or "redis://localhost:6379/0")
    celery_app.conf.update(
        task_serializer="json",
        result_serializer="json",
        accept_content=["json"],
        task_track_started=True,
        worker_prefetch_multiplier=1,
        task_acks_late=True,
    )

    @celery_app.task(name="lis_dashboard.execute_job")
    def execute_job(job_id: str, kind: str, payload: dict):
        from app.schemas import BatchDifferenceRequest, UnsupervisedRequest
        from app.services.batch_difference import run_batch_difference
        from app.services.clustering import run_unsupervised
        from app.services.data_store import apply_filter, dataset_store

        job_store.update(job_id, kind=kind, status="running", progress=0.0, detail="worker 开始计算")
        try:
            dataset_id = payload["dataset_id"]
            request_data = payload["request"]
            if kind == "unsupervised":
                request = UnsupervisedRequest(**request_data)
                frame = dataset_store.apply_filter(dataset_id, request)
                result = run_unsupervised(frame, request.grouping_col, request.cluster_num,
                                          request.model_name, request.embedding_dim, request.instruction,
                                          progress_callback=lambda value, detail: job_store.progress(job_id, value, detail))
            elif kind == "batch_difference":
                request = BatchDifferenceRequest(**request_data)
                indexed = dataset_store.frame(dataset_id).with_row_index("_source_row_index")
                frame = apply_filter(indexed, request)
                result = run_batch_difference(
                    frame,
                    request.formula,
                    request.n_value,
                    request.step_value,
                    progress_callback=lambda value, detail: job_store.progress(job_id, value, detail),
                )
                result["dataset_id"] = dataset_id
                if isinstance(result, dict) and "_artifact" in result:
                    job_store.save_artifact(job_id, result.pop("_artifact"))
                    result["artifact_available"] = True
            else:
                raise ValueError(f"未知任务类型: {kind}")
            return job_store.update(job_id, status="completed", progress=1.0, detail="计算完成", result=result)
        except Exception as exc:
            job_store.update(job_id, status="failed", detail="计算失败", error=f"{type(exc).__name__}: {exc}")
            raise
else:
    celery_app = None
    execute_job = None
