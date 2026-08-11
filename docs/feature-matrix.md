# R Shiny → FastAPI/Vue 功能映射

| 旧页面/模块 | Python/Vue 入口 | 状态 |
|---|---|---|
| 数据上传 | `POST /api/datasets/upload` + 当前数据页 | 已实现 |
| 缺失/无效值筛选 | `FilterRequest` + 左侧筛选器 | 已实现 |
| 当前数据、CSV 下载 | `/api/datasets/{id}/rows`、`download` | 已实现 |
| 关键词聚类 | `/api/clustering/{id}/keyword/*` | 已实现 |
| 词向量无监督聚类 | `/api/clustering/{id}/unsupervised` | 已实现，Qwen3-Embedding-4B + 本地模型目录 + SQLite 缓存 |
| 数值变量离散化 | `/api/analysis/{id}/histogram`、`discretize` | 已对齐旧版：Box-Cox/原数据、直方图、分位数表、离散分组图/表及覆盖写入 |
| 探索性定性分析 | `pie`、`discrete-stack`、`consecutive-stack` | 已对齐旧版：饼图、离散堆叠、上下两个连续堆叠图、Top-N、数量/比例、颜色和排序 |
| 探索性定量分析 | `quantitative-trend` | 已对齐旧版：Box-Cox、400×IQR、年龄窗口、分位数上下界、误差线、散点和 hover |
| 批间差异分析 | `/api/batch-difference/{id}/run` + worker | 已对齐旧版：公式校验、滑动窗口统计、显著性图、原始数据/密度/富集详情和自动结论 |
| 参考区间与人群异质性评估 | `dev_raw/数据看板.Rmd` 中的历史代码 | 当前 R UI 仍是占位页，待确认是否纳入迁移范围 |
| 分位数回归协方差分析 | `dev_raw/数据看板.Rmd` 中的历史代码 | 当前 R UI 未接入，待确认是否纳入迁移范围 |

## 性能约束

1. 原始数据保存为 Parquet，前端只接收分页行或聚合图表数据。
2. 嵌入只处理唯一短语，向量按模型/维度/短语哈希写入 SQLite BLOB，不产生几十万个小文件。
3. 聚类使用样本权重，不复制原始样本行。
4. 聚类中心近邻搜索优先使用 FAISS，fallback 按块计算。
5. 向量化和批间差异分析通过 Celery worker 执行，API 主进程不被长计算阻塞。
