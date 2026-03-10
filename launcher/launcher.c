#include <windows.h>
#include <shlobj.h>
#include <stdio.h>
#include <unistd.h>

// 宏定义配置
#define APP_NAME "LIS Dashboard"
#define R_PATH "R-Portable\\bin\\x64\\Rscript.exe"
#define R_SCRIPT "launch.R"
#define ICON_FILE "logo.ico"

// 函数：创建桌面快捷方式 (替代原 .bat 的 VBS 部分)
void CreateShortcut() {
  char desktopPath[MAX_PATH];
  char exePath[MAX_PATH];
  char workDir[MAX_PATH];
  char linkPath[MAX_PATH];
  
  // 获取当前 exe 的绝对路径和工作目录
  GetModuleFileNameA(NULL, exePath, MAX_PATH);
  GetCurrentDirectoryA(MAX_PATH, workDir);
  
  // 获取桌面路径
  SHGetSpecialFolderPathA(HWND_DESKTOP, desktopPath, CSIDL_DESKTOP, FALSE);
  sprintf(linkPath, "%s\\%s.lnk", desktopPath, APP_NAME);
  
  // 如果快捷方式已存在，就不再重复创建
  if (access(linkPath, 0) == 0) return;
  
  // 初始化 COM 库
  CoInitialize(NULL);
  IShellLinkA* psl;
  HRESULT hr = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, &IID_IShellLinkA, (LPVOID*)&psl);
  
  if (SUCCEEDED(hr)) {
    IPersistFile* ppf;
    psl->lpVtbl->SetPath(psl, exePath);
    psl->lpVtbl->SetWorkingDirectory(psl, workDir);
    psl->lpVtbl->SetShowCmd(psl, SW_SHOWMINNOACTIVE); // 窗口最小化运行
    
    // 设置图标 (如果存在)
    char iconPath[MAX_PATH];
    sprintf(iconPath, "%s\\%s", workDir, ICON_FILE);
    if (access(iconPath, 0) == 0) {
      psl->lpVtbl->SetIconLocation(psl, iconPath, 0);
    }
    
    hr = psl->lpVtbl->QueryInterface(psl, &IID_IPersistFile, (LPVOID*)&ppf);
    if (SUCCEEDED(hr)) {
      WCHAR wsz[MAX_PATH];
      MultiByteToWideChar(CP_ACP, 0, linkPath, -1, wsz, MAX_PATH);
      ppf->lpVtbl->Save(ppf, wsz, TRUE);
      ppf->lpVtbl->Release(ppf);
    }
    psl->lpVtbl->Release(psl);
  }
  CoUninitialize();
}

int main() {
  // 1. 自动维护：创建快捷方式
  CreateShortcut();
  
  // 2. 获取当前工作目录（portable 根目录）
  char workDir[MAX_PATH];
  GetCurrentDirectoryA(MAX_PATH, workDir);

  // 3. 准备启动 R 进程
  char cmd[MAX_PATH * 2];
  sprintf(cmd, "%s %s", R_PATH, R_SCRIPT);
  
  STARTUPINFOA si;
  PROCESS_INFORMATION pi;
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  
  // 设置隐藏窗口 (SW_HIDE)，彻底干掉黑框
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE; 
  
  ZeroMemory(&pi, sizeof(pi));
  
  // 4. 启动 R 进程 (它是核心，IT 的白名单就认它)
  if (CreateProcessA(NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi)) {
    // 5. 在后台启动 GUI viewer（dashboard_viewer.exe）
    char viewerPath[MAX_PATH];
    char viewerCmd[MAX_PATH * 2];

    // 优先在根目录查找 dashboard_viewer.exe
    sprintf(viewerPath, "%s\\dashboard_viewer.exe", workDir);
    if (access(viewerPath, 0) != 0) {
      // 若不存在，则尝试 gui 子目录
      sprintf(viewerPath, "%s\\gui\\dashboard_viewer.exe", workDir);
    }

    if (access(viewerPath, 0) == 0) {
      STARTUPINFOA si_viewer;
      PROCESS_INFORMATION pi_viewer;
      ZeroMemory(&si_viewer, sizeof(si_viewer));
      si_viewer.cb = sizeof(si_viewer);
      // viewer 本身是 GUI 程序，不需要控制台窗口
      ZeroMemory(&pi_viewer, sizeof(pi_viewer));

      // 端口固定为 8888，与 launch.R 中保持一致
      sprintf(viewerCmd, "\"%s\" 8888", viewerPath);
      CreateProcessA(
        NULL,
        viewerCmd,
        NULL,
        NULL,
        FALSE,
        0,
        NULL,
        NULL,
        &si_viewer,
        &pi_viewer
      );

      // 我们不等待 GUI 进程结束，只需关闭句柄
      if (pi_viewer.hProcess != NULL) {
        CloseHandle(pi_viewer.hProcess);
      }
      if (pi_viewer.hThread != NULL) {
        CloseHandle(pi_viewer.hThread);
      }
    }

    // 【关键改动】：让 Launcher 不要立刻退出，而是等待 R 进程结束
    // 这样在 R 运行期间，Launcher 始终是它的有效父进程
    WaitForSingleObject(pi.hProcess, INFINITE); 
    
    // 只有当 R 关闭了，Launcher 才关闭
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  }
  
  // 清理句柄并退出 (父进程功成身退)
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  
  return 0;
}
 