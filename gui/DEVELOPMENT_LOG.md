# LIS Dashboard GUI 开发日志

## 背景

LIS 数据看板是一个 R Shiny 应用，需要桌面 GUI 包装器来绕过公司 IT 的加密文件权限管控。加密权限与 Windows 进程名绑定——只有白名单中的进程才能读取加密文件。

### 第一代方案（Go + WebView2）— 废弃

- **技术栈**：Go + `github.com/webview/webview_go`（Windows WebView2 内核）
- **工作方式**：Go 程序创建 WebView2 窗口，指向 `http://127.0.0.1:8888`（Shiny 后端）
- **进程名**：`dashboard_viewer.exe`
- **为什么失败**：WebView2 的运行时进程 `msedgewebview2.exe` 是系统级共享的。WhatsApp 也使用 WebView2 内核，导致 WhatsApp 的 `msedgewebview2.exe` 进程被 IT 安全软件认为"也能读加密文件"，IT 关闭了白名单权限。

### 第二代方案（C# WinForms + CefSharp / CEF）— 当前方案

- **技术栈**：C# WinForms (.NET Framework 4.6.2) + CefSharp (CEF/Chromium 内嵌浏览器)
- **工作方式**：C# WinForms 窗口内嵌 ChromiumWebBrowser 控件，指向 Shiny 后端
- **进程名**：所有进程（主进程 + 渲染器 + GPU 等子进程）全部复用同一个 `dashboard_viewer.exe`
- **为什么有效**：CEF 不像 WebView2 那样依赖系统共享运行时，所有子进程都通过 `BrowserSubprocessPath = Application.ExecutablePath` 指向宿主 exe 自身，不存在任何与其它软件重合的进程名

## 技术路线决策

### 1. 为什么选择 CEF 而不是 WebView2 Fixed Version？

WebView2 支持 Fixed Version（捆绑私有运行时），但：
- 即使捆绑私有副本，进程名仍然是 `msedgewebview2.exe`（数字签名绑定，无法改名）
- 公司 IT 安全软件按进程 **名称** 匹配白名单，不区分进程路径
- 因此即使 `msedgewebview2.exe` 来自不同目录，白名单仍然会溢出给 WhatsApp

CEF 的 `BrowserSubprocessPath` 机制允许指定子进程使用宿主 exe 自身，实现了进程名的真正唯一。

### 2. 为什么选择 .NET Framework 4.6.2 而不是 .NET 8？

| 因素 | .NET 8 | .NET Framework 4.6.2 |
|------|--------|----------------------|
| CefSharp 兼容性 | CefSharp 126 只有 net462 TFM，.NET 8 靠兼容模式跑 | 原生支持，无 NU1701 警告 |
| 目标机器依赖 | 需装 .NET 8 Runtime 或 self-contained（+70MB） | Windows 10/11 内置 .NET Framework 4.8 |
| 输出体积 | ~240MB (self-contained + CEF) | ~170MB (CEF only) |
| C# 语言特性 | 支持最新特性 | 需设置 LangVersion=10.0 |

**结论**：选 net462，减少兼容性风险，目标机器无需额外安装。

### 3. 为什么选择 WinForms 而不是 WPF？

- WinForms 更简单，`ChromiumWebBrowser` 直接作为控件嵌入
- 不需要 XAML 布局，纯代码创建窗口
- 窗口功能简单（只有一个全屏浏览器），不需要 WPF 的复杂绑定能力

### 4. 为什么选择 CefSharp 而不是直接调 CEF C++ API？

- CefSharp 提供 WinForms 控件 `ChromiumWebBrowser`，开箱即用
- CEF C++ API 需要大量 P/Invoke 或 C++/CLI 封装
- CefSharp 社区活跃，NuGet 包维护良好

## 踩坑记录

### 坑 1：`BrowserSubprocessPath` 导致 CEF 初始化卡死（伪误报）

**现象**：设置 `BrowserSubprocessPath = Application.ExecutablePath` 后，进程存活但窗口不出来（MainWindowHandle = 0）。

**排查过程**：
1. 怀疑是 CEF 子进程启动失败 → 去掉 BrowserSubprocessPath 测试，仍然失败
2. 怀疑是 .NET Framework 兼容性问题 → 换成测试项目，发现纯 WinForms 窗口正常
3. 怀疑是 CEF 自身初始化问题 → 用 `about:blank` 测试，窗口正常
4. 逐步二分，定位到 **WaitForServer（连接 Shiny 检测端口）** 是真正元凶

**根因**：`HttpWebRequest` 初始化 .NET HTTP 堆栈（ServicePointManager 连接池）后，会干扰 CEF/Chromium 自身网络栈的初始化。CEF 初始化时 Chromium 网络栈无法正常工作，窗口创建失败但不抛异常。

**解决方案**：用 `TcpClient.Connect("127.0.0.1", port)` 替代 `HttpWebRequest`，只做原始 TCP 端口检测，不碰 HTTP 协议层。

```csharp
// ❌ 会干扰 CEF 网络栈
var request = (HttpWebRequest)WebRequest.Create(url);
using (var response = (HttpWebResponse)request.GetResponse()) { ... }

// ✅ 纯 TCP 检测，不干扰 CEF
using (var tcp = new TcpClient())
{
    tcp.Connect("127.0.0.1", port);
    return true;
}
```

### 坑 2：`net8.0-windows` 与 CefSharp 的 TFM 不匹配

**现象**：CefSharp.WinForms 126.2.70 只有 `lib/net462` 目录，NuGet 以 NU1701 警告降级到 .NET Framework 兼容模式。

**解决**：TargetFramework 从 `net8.0-windows` 改为 `net462`。

### 坑 3：`PlatformTarget` 必须指定 x64

**现象**：`CefSharp.Common.targets(373,5): error : CefSharp.Common is unable to proceed as your current PlatformTarget is 'AnyCPU'`

**原因**：CEF 是原生 C++ 库，分 x86 和 x64 版本，不支持 AnyCPU。

**解决**：csproj 中设置 `<PlatformTarget>x64</PlatformTarget>`。

### 坑 4：C# 语言版本与 Nullable / ImplicitUsings

**现象**：net462 默认 C# 7.3，不支持 `Nullable enable`、`ImplicitUsings enable`、file-scoped namespace。

**解决**：
- 设置 `<LangVersion>10.0</LangVersion>` 支持 modern C# 特性
- 去掉 `<Nullable>enable</Nullable>`（Nullable 运行时属性在 net462 中不完全可用）
- 使用传统 `namespace X { }` 格式

### 坑 5：launcher.c 的 COM GUID 链接错误

**现象**：`undefined reference to IID_IShellLinkA` 和 `IID_IPersistFile`

**原因**：这些 COM 接口 GUID 定义在 `libuuid.a` 中，GCC 默认不链接。

**解决**：编译命令加 `-luuid`：
```bat
gcc launcher.c -o launcher.exe -lole32 -lshell32 -luuid -mwindows
```

### 坑 6：快捷方式不重建

**现象**：`CreateShortcut()` 中 `if (access(linkPath, 0) == 0) return;` 导致旧快捷方式存在时跳过创建，移动目录后快捷方式指向错误路径。

**解决**：改为每次删除旧快捷方式再重建：
```c
if (access(linkPath, 0) == 0) {
    DeleteFileA(linkPath);
}
```

### 坑 7：WSL 与 Windows 的路径/编译交互

**问题**：MSBuild 不支持 WSL UNC 路径 (`\\wsl.localhost\...`)，`cp` 通过 WSL↔Windows 9P 文件系统有时不可靠，PowerShell 从 WSL 调用时 `$` 变量被 bash 展开。

**解决**：编译时先用 `cp` 复制到 `/mnt/c/temp/`（Windows 本地路径），再用 `dotnet` 从 Windows 路径编译。shell 中 PowerShell 变量用 `\$` 转义或用脚本文件。

## 编译部署流程

### 编译（在 Windows 上）

```bat
# 1. GUI (浏览器包装器)
cd gui
build.bat
# 输出到 gui\publish\

# 2. Launcher (C 启动器)
cd launcher
gcc launcher.c -o launcher.exe -lole32 -lshell32 -luuid -mwindows
```

### 部署结构

```
LIS_Dashboard_便携包/
├── launcher.exe              # C 启动器
├── dashboard_viewer.exe      # C# CEF GUI（主程序）
├── CefSharp*.dll             # CEF C# 绑定
├── libcef.dll                # Chromium Embedded Framework（~216MB）
├── chrome_elf.dll, *.pak, *.dat, *.bin  # CEF 资源文件
├── locales/                  # CEF 本地化
├── R-Portable/               # 便携 R 运行时
├── launch.R                  # R 启动脚本
└── logo.ico                  # 图标
```

### 依赖项

| 依赖 | 编译时需要 | 运行时需要 |
|------|-----------|-----------|
| .NET 8 SDK | ✅ | ❌ |
| .NET Framework 4.6.2+ | ❌ | ✅（Win10/11 内置） |
| GCC (MinGW) | ✅ | ❌ |
| Visual C++ Redistributable | ❌ | ✅（通常已安装） |
| R-Portable | ❌ | ✅ |

## 进程架构

```
launcher.exe                            ← C 启动器，创建快捷方式 + 启动子进程
├── Rscript.exe                         ← R 后端 (Shiny on port 8888)
│   └── (读取加密 Excel 文件)
└── dashboard_viewer.exe                ← CEF 主进程 (WinForms 窗口)
    ├── dashboard_viewer.exe --type=gpu-process     ← CEF GPU 进程
    └── dashboard_viewer.exe --type=renderer        ← CEF 渲染进程 (xN)
```

**关键**：CEF 所有子进程通过 `BrowserSubprocessPath` 指向宿主 exe，进程名全部是 `dashboard_viewer.exe`。IT 只需白名单这一个进程名。

## 关键代码片段

### CEF 唯一进程名配置 (Program.cs)

```csharp
var settings = new CefSettings
{
    // 所有 CEF 子进程都复用宿主 exe → 唯一进程名
    BrowserSubprocessPath = Application.ExecutablePath,
    // 独立的缓存和用户数据目录
    RootCachePath = Path.Combine(appData, "CEF"),
    CachePath     = Path.Combine(appData, "CEF", "Cache"),
};
```

### 端口检测 (Program.cs)

```csharp
// 用 TCP 而非 HTTP——避免 .NET HTTP 堆栈干扰 CEF
private static bool WaitForServer(int port, int timeoutSecs)
{
    var deadline = DateTime.Now.AddSeconds(timeoutSecs);
    while (DateTime.Now < deadline)
    {
        try
        {
            using (var tcp = new System.Net.Sockets.TcpClient())
            {
                tcp.Connect("127.0.0.1", port);
                return true;
            }
        }
        catch { Thread.Sleep(200); }
    }
    return false;
}
```

### CEF 子进程检测 (Program.cs)

```csharp
// 必须放在 Main() 最开头——CEF 子进程会带着 --type=renderer 参数重新进入
var exitCode = Cef.ExecuteProcess();
if (exitCode >= 0) return exitCode;  // 子进程在这里退出
```
