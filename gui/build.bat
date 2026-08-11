@echo off
REM LIS Dashboard GUI - CEF-based browser wrapper
REM Build script for Windows (requires .NET 8 SDK for the compiler)
REM Download: https://dotnet.microsoft.com/en-us/download/dotnet/8.0
REM
REM Targets .NET Framework 4.6.2 (built into Windows 10/11) -
REM no extra runtime needed on the target machine.

cd /d "%~dp0csharp"

echo [1/2] Restoring NuGet packages (including CEF binaries)...
dotnet restore
if errorlevel 1 (
    echo ERROR: dotnet restore failed.
    echo Make sure .NET SDK is installed.
    pause
    exit /b 1
)

echo [2/2] Building and publishing...
dotnet publish -c Release -o ..\publish
if errorlevel 1 (
    echo ERROR: Build failed.
    pause
    exit /b 1
)

echo.
echo ============================================
echo  Build successful!
echo.
echo  Output directory: gui\publish\
echo  Main executable:  gui\publish\dashboard_viewer.exe
echo.
echo  ALL processes in Task Manager will appear as
echo  "dashboard_viewer.exe" - no overlap with
echo  msedgewebview2.exe, WhatsApp, or any other app.
echo.
echo  Copy the entire contents of gui\publish\ to
echo  the deployment root (alongside launcher.exe).
echo ============================================
pause
