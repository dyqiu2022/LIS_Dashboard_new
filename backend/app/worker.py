"""Compatibility worker entrypoint.

Production Docker uses Celery directly (see docker-compose.yml). This module keeps a clear
local command for developers who only want to verify that the worker dependencies import.
"""

from app.config import settings


if __name__ == "__main__":
    if not settings.redis_url:
        raise SystemExit("请设置 LIS_REDIS_URL 后运行 Celery worker")
    print("Use: celery -A app.celery_app.celery_app worker --loglevel=INFO --concurrency=1")
