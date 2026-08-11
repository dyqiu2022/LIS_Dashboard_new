# WSL 原生 GPU 部署

该部署只让 FastAPI 和前端在 WSL 原生运行，不依赖 Rancher Desktop 的 GPU 容器支持；R Shiny、GUI 和其他 Docker 服务保持不变。

模型权重仍位于 `/mnt/c/LIS_Dashboard/models`（Windows 的 `C:\LIS_Dashboard\models`），运行时使用 `LIS_MODEL_LOCAL_ONLY=1`。

## 启动

在仓库根目录执行：

```bash
# 使用现有 node_modules 缓存构建当前前端
cd frontend
VITE_API_BASE=http://localhost:18000 npm run build
cd ..

chmod +x deployment/wsl-gpu/*.sh
deployment/wsl-gpu/start.sh
```

默认使用 `F2LLM-v2-4B`；用户可在页面上切换到 `F2LLM-v2-1.7B`。任务在 API 进程内串行执行（`LIS_LOCAL_JOBS=1`），避免多个进程重复占用显存。

```bash
deployment/wsl-gpu/stop.sh
```

运行日志和 PID 位于 `~/.local/share/lis-dashboard/{logs,run}`，数据位于 `~/.local/share/lis-dashboard/data`。
