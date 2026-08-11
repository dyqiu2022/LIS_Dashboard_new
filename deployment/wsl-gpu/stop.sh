#!/usr/bin/env bash
set -euo pipefail

RUNTIME_DIR="${LIS_RUNTIME_DIR:-$HOME/.local/share/lis-dashboard}"
RUN_DIR="$RUNTIME_DIR/run"

stop_process() {
  local name="$1"
  local pid_file="$RUN_DIR/$name.pid"
  [[ -f "$pid_file" ]] || return 0
  local pid
  pid="$(cat "$pid_file")"
  if kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    for _ in $(seq 1 20); do
      kill -0 "$pid" 2>/dev/null || break
      sleep 0.25
    done
    kill -9 "$pid" 2>/dev/null || true
    echo "stopped $name (pid $pid)"
  fi
  rm -f "$pid_file"
}

stop_process web
stop_process api
