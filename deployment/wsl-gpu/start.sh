#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
VENV="${LIS_PYTHON_VENV:-$HOME/.venvs/lis-dashboard}"
RUNTIME_DIR="${LIS_RUNTIME_DIR:-$HOME/.local/share/lis-dashboard}"
DATA_DIR="${LIS_DATA_DIR:-$RUNTIME_DIR/data}"
MODEL_DIR="${LIS_MODEL_DIR:-/mnt/c/LIS_Dashboard/models}"
RUN_DIR="$RUNTIME_DIR/run"
LOG_DIR="$RUNTIME_DIR/logs"
mkdir -p "$DATA_DIR" "$RUN_DIR" "$LOG_DIR"

if [[ ! -x "$VENV/bin/python" ]]; then
  echo "Python 虚拟环境不存在: $VENV" >&2
  exit 1
fi
if [[ ! -x "$ROOT_DIR/frontend/node_modules/.bin/vite" ]]; then
  echo "前端依赖不存在，请先在 frontend 执行 npm install" >&2
  exit 1
fi

export PYTHONPATH="$ROOT_DIR/backend"
export PYTHONUNBUFFERED=1
export TOKENIZERS_PARALLELISM=false
export LIS_DATA_DIR="$DATA_DIR"
export LIS_MODEL_DIR="$MODEL_DIR"
export HF_HOME="$MODEL_DIR/huggingface"
export HUGGINGFACE_HUB_CACHE="$MODEL_DIR/huggingface/hub"
export LIS_EMBEDDING_MODEL="${LIS_EMBEDDING_MODEL:-codefuse-ai/F2LLM-v2-4B}"
export LIS_EMBEDDING_DIM="${LIS_EMBEDDING_DIM:-1024}"
export LIS_EMBEDDING_BATCH_SIZE="${LIS_EMBEDDING_BATCH_SIZE:-64}"
export LIS_EMBEDDING_DEVICE="cuda"
export LIS_EMBEDDING_DTYPE="bfloat16"
export LIS_MODEL_LOCAL_ONLY=1
export LIS_LOCAL_JOBS=1
export LIS_REDIS_URL=""
export LIS_ALLOW_ORIGINS="http://localhost:18080,http://127.0.0.1:18080"
# Triton may compile a tiny CUDA helper on first use.
export CPATH="${CPATH:-$HOME/.local/opt/libpython3.12-dev/usr/include/python3.12:$HOME/.local/opt/libpython3.12-dev/usr/include/x86_64-linux-gnu/python3.12}"

start_process() {
  local name="$1"
  shift
  local pid_file="$RUN_DIR/$name.pid"
  if [[ -f "$pid_file" ]]; then
    local old_pid
    old_pid="$(cat "$pid_file")"
    if kill -0 "$old_pid" 2>/dev/null; then
      echo "$name already running (pid $old_pid)"
      return
    fi
    rm -f "$pid_file"
  fi
  nohup "$@" >"$LOG_DIR/$name.log" 2>&1 < /dev/null &
  echo $! > "$pid_file"
  echo "started $name (pid $!)"
}

cd "$ROOT_DIR/backend"
start_process api "$VENV/bin/python" -m uvicorn app.main:app --host 0.0.0.0 --port 18000
cd "$ROOT_DIR/frontend"
start_process web "$ROOT_DIR/frontend/node_modules/.bin/vite" preview --host 0.0.0.0 --port 18080

for _ in $(seq 1 60); do
  if curl -fsS http://127.0.0.1:18000/healthz >/dev/null 2>&1; then
    echo "API ready: http://127.0.0.1:18000"
    echo "Web ready: http://127.0.0.1:18080"
    exit 0
  fi
  sleep 1
done

echo "API 未在 60 秒内就绪，请查看 $LOG_DIR/api.log" >&2
exit 1
