# === 智能部署/启动脚本 (boot.R) ===

# 1. 设置库路径 (确保只认我们自己的库，防止去读系统的)
# 假设 boot.R 在根目录，library 在当前目录下的 library 文件夹

root_dir <- getwd()

my_lib <- file.path(root_dir, "R-Portable/library")
message("--- 部署/启动系统加载中 ---")
message("当前库路径: ", my_lib)

# 2. 扫描是否有待安装的源码包 (.tar.gz)
# 我们约定：把更新包直接放在根目录
pkg_files <- list.files(my_lib, pattern = "lisdashboard_.*\\.tar\\.gz$", full.names = TRUE)

if (length(pkg_files) > 0) {
  # 如果发现了更新包，通常取最新的一个（或者只有一个）
  target_pkg <- pkg_files[1] 
  
  message("\n发现新版本安装包，开始现场施工...")
  message("正在安装: ", basename(target_pkg))
  
  # 3. 现场编译安装
  # type = "source" 是核心，它会在当前电脑上重新生成所有路径
  tryCatch({
    install.packages(
      target_pkg, 
      repos = NULL, 
      type = "source", 
      lib = my_lib,
      # 加上这个防止安装时被锁住
      INSTALL_opts = c("--no-lock") 
    )
    
    message("安装成功！")
    
    # 4. 安装完后“毁尸灭迹”
    # 删掉包，防止下次启动重复安装浪费时间
    unlink(target_pkg)
    message("安装包已清理。")
    
  }, error = function(e) {
    message("【严重错误】安装失败！")
    message(e)
    # 如果安装失败，暂停一下让用户看到报错
    Sys.sleep(10)
    quit(save = "no")
  })
  
} else {
  message("未发现安装包，准备直接启动...")
}

# 5. 启动应用
if (require("lisdashboard", lib.loc = my_lib)) {
  message("\n正在唤醒 Lisdashboard...")
  
  # === 邱工配置区 ===
  my_port <- 8888  # 固定端口，与 dashboard_viewer.exe 中保持一致
  
  # 1. 设置端口 (不再随机乱跳)
  options(shiny.port = my_port)
  
  # 2. 不自动弹出浏览器，由外部 GUI viewer 负责展示
  options(shiny.launch.browser = FALSE)
  
  # 3. (可选) 设置 host
  # "127.0.0.1" = 只有这台电脑能看 (安全)
  # "0.0.0.0"   = 同一局域网的同事也能通过您的IP访问 (共享)
  options(shiny.host = "127.0.0.1")
  
  message(paste0(">>> 启动成功！请访问: http://127.0.0.1:", my_port))
  message(">>> 轻量级 GUI 窗口由外部启动器负责调用 dashboard_viewer.exe")

  # 只负责启动 Shiny 服务，GUI viewer 由 C 启动器管理
  lisdashboard::run_app()
  
} else {
  message("【错误】未找到 lisdashboard 包，且没有安装包！请联系邱东艺。")
  Sys.sleep(10)
}
