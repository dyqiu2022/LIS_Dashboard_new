#!/usr/bin/env Rscript
# 简化版运行脚本 - 快速启动应用
# 使用方法: source("run_app_simple.R")

# 加载必要的包
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools", repos = "https://cran.rstudio.com/")
}

# 加载包
devtools::load_all()

# 运行应用
cat("正在启动应用...\n")
run_app()
