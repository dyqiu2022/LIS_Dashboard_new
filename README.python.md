# LIS Dashboard Python/Vue 重写版

新版本与旧 R Shiny 代码并行保留，目录如下：

- `backend/`：FastAPI、Polars、统计计算和向量服务
- `frontend/`：Vue 3 看板
- `docker-compose.yml`：Docker 直接部署
- `深圳市龙岗区人民医院 CA125(已清洗).xlsx`：验收样本

旧的 `R/`、`gui/`、`launcher/` 仅作为功能和结果对照，不参与新 Docker 启动流程。

## 本地运行

```bash
docker compose up --build
# 浏览器打开 http://localhost:8080
```

词向量默认使用支持指令提示词的 `codefuse-ai/F2LLM-v2-4B`，界面也可选择更快的 `F2LLM-v2-1.7B`（输出统一截断为 1024 维）。模型不会写入项目目录：WSL 原生 GPU 部署和 Rancher Desktop 部署都从宿主机 `C:\LIS_Dashboard\models` 加载。当前测试机推荐直接使用 WSL 原生 PyTorch CUDA，避免 Rancher Desktop 的 GPU 容器限制；普通无 GPU 部署仍可使用 CPU-only PyTorch。

```bash
docker compose -f docker-compose.yml -f docker-compose.gpu.yml up --build
```

首次启动前先预热模型：

```bash
docker compose run --rm -e LIS_MODEL_LOCAL_ONLY=0 api python -m app.prewarm_model
```

Rancher Desktop 专用部署文件为 `docker-compose.rancher.yml`。WSL 原生 GPU 启动脚本位于 `deployment/wsl-gpu/start.sh`，它使用单个 API 进程串行执行 GPU 任务，模型缺失时直接报错，不会偷偷联网下载。模型预热和离线运行均使用 `LIS_MODEL_LOCAL_ONLY=1`。

## IVD 平台集成

IVD 平台的统一 WSL Docker Engine 编排位于 `SNB_manage_system/docker-compose.yml`，通过 `/lis-dashboard/` 提供本系统。集成镜像必须使用以下构建参数，使静态资源和 API 都保持在独立路径下：

```bash
docker build --pull=false -f backend/Dockerfile.gpu -t lis_dashboard_api:2026_08_11_10_07 backend
docker build --pull=false --build-arg VITE_BASE_PATH=/lis-dashboard/ --build-arg VITE_API_BASE=/lis-dashboard/api -t lis_dashboard_web:2026_08_11_10_07 frontend
```

统一编排默认只读挂载 `/mnt/c/LIS_Dashboard/models`，并复用 `~/.local/share/lis-dashboard/data`。运行阶段的 Compose 服务均配置为 `pull_policy: never`，不会重新拉取镜像。

## 旧版分析功能对齐

03–06 已按 `R/` 中的 Shiny 模块对齐：

- 03 离散化：原数据/Box-Cox、直方图、分位数表、离散分组图和分组统计表。
- 04 定性分析：饼图、离散交叉堆叠图、采样时间/年龄/定量结果连续堆叠图、Top-N、数量/比例、颜色和排序。
- 05 定量分析：400×IQR 截断、Box-Cox、年龄滑动窗口、分位数上下界、原始散点、悬停模式和误差线。
- 06 批间差异：公式校验、滑动窗口统计、BH 调整、显著性颜色、批号信息以及点击后的原始数据、密度图、协变量富集和自动结论。

批间任务的点击详情 artifact 保存在 `LIS_DATA_DIR/jobs/`，不写入项目目录，也不包含模型文件。
