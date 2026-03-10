# LIS 数据看板 - 轻量级 GUI 窗口

替代浏览器的启动窗口，使用系统自带的 WebView（Windows = WebView2）。

## 特性

- 极简界面：只显示页面，无地址栏、收藏栏等
- Ctrl + 鼠标滚轮：调节页面缩放（50%–200%）
- 轻量：单个 exe，依赖系统 WebView2（Win10/11 已内置）

## 编译（Windows）

1. 安装 Go：https://go.dev/dl/
2. 安装 GCC（webview 需 CGO）：MinGW-w64 或 MSYS2
3. 在 `gui` 目录执行：

```bash
cd gui
go mod tidy
go build -ldflags "-s -w -H windowsgui" -o dashboard_viewer.exe
```

- `-H windowsgui`：无控制台窗口
- `-s -w`：减小体积

编译完成后，将 `dashboard_viewer.exe` 放到与 `launcher.exe` 同一目录。

## 使用方式

由 `launch.R` 在 Shiny 启动时自动调用，无需手动运行。

若需手动测试：

```bash
# 先启动 Shiny（端口 8888），再执行：
dashboard_viewer.exe 8888
```
