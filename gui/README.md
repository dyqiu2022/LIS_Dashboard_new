# LIS 数据看板 - 桌面 GUI 窗口 (CEF 版本)

替代浏览器的启动窗口，使用 CEF (Chromium Embedded Framework) 内嵌浏览器。

## 为什么用 CEF 替代 WebView2

WebView2 的运行时进程 (`msedgewebview2.exe`) 是系统级共享的 —— WhatsApp 等软件也使用同样的进程。这导致 IT 白名单无法区分不同软件的 WebView2 进程。

CEF 的解决方案：**所有子进程都复用主程序自身**（`dashboard_viewer.exe`），进程管理器里不会有任何与其它软件重合的进程名。

## 特性

- 极简界面：只显示页面，无地址栏、收藏栏等
- Ctrl + 鼠标滚轮：调节页面缩放（50%–200%）
- **进程隔离**：所有 CEF 进程均显示为 `dashboard_viewer.exe`，不依赖系统共享运行时

## 编译（Windows）

### 前置条件

1. 安装 .NET 8 SDK：https://dotnet.microsoft.com/en-us/download/dotnet/8.0
2. 无需安装 GCC 或其他依赖（CEF 二进制文件通过 NuGet 自动下载）

### 编译步骤

在 `gui` 目录执行：

```bat
build.bat
```

这会自动：
1. 还原 NuGet 包（包括 CEF 二进制文件）
2. 以 win-x64 self-contained 模式发布
3. 输出到 `gui\publish\` 目录

### 部署

将 `gui\publish\` 目录下的**所有文件**复制到便携包根目录（与 `launcher.exe` 同级），或保留在 `gui\publish\` 目录中（launcher 会自动查找）。

## 使用方式

由 `launcher.exe` 在启动时自动调用。launcher 按以下顺序查找 `dashboard_viewer.exe`：

1. 根目录 (`dashboard_viewer.exe`)
2. `gui\publish\` 目录
3. `gui\` 目录

若需手动测试：

```bat
# 先启动 Shiny（端口 8888），再执行：
dashboard_viewer.exe 8888
```

## 旧版本 (Go + WebView2)

旧版 Go + WebView2 代码保留在仓库中：

- `main.go` - Go 源码
- `go.mod` / `go.sum` - Go 依赖
- `build_go.bat` - Go 编译脚本

如需回退到 WebView2 版本，使用 `build_go.bat` 编译。
