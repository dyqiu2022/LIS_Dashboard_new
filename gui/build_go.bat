@echo off
REM LIS Dashboard 轻量级 GUI 编译脚本 (Windows)
REM 需要: Go 1.21+ 和 GCC (MinGW/MSYS2)
cd /d "%~dp0"

go mod tidy
if errorlevel 1 (
  echo 错误: go mod tidy 失败
  pause
  exit /b 1
)

go build -ldflags "-s -w -H windowsgui" -o dashboard_viewer.exe
if errorlevel 1 (
  echo 错误: 编译失败
  pause
  exit /b 1
)

echo 编译成功: dashboard_viewer.exe
echo 请将其复制到便携包根目录 (与 launcher.exe 同级)
pause
