FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    TOKENIZERS_PARALLELISM=false \
    HF_HOME=/models/huggingface \
    HUGGINGFACE_HUB_CACHE=/models/huggingface/hub

WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential python3-dev libgomp1 \
    && rm -rf /var/lib/apt/lists/*
COPY pyproject.toml requirements.txt ./
RUN pip install --upgrade pip \
    && pip install --index-url https://download.pytorch.org/whl/cpu "torch>=2.4,<3" \
    && pip install -e '.[worker]'
COPY app ./app
RUN mkdir -p /data /models
EXPOSE 8000
CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]
