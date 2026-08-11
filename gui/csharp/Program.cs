using CefSharp;
using CefSharp.WinForms;

namespace LISDashboard
{

static class Program
{
    [STAThread]
    static int Main(string[] args)
    {
        // CEF subprocess detection - must be the first thing we do.
        // When CEF launches subprocesses, they re-enter this Main() with
        // special args like --type=renderer.  ExecuteProcess handles those
        // and returns a non-negative exit code so the subprocess exits cleanly.
        var exitCode = Cef.ExecuteProcess();
        if (exitCode >= 0)
            return exitCode;

        // --- Main process only below this line ---

        int port = 8888;
        if (args.Length > 0 && int.TryParse(args[0], out var p) && p > 0 && p < 65536)
            port = p;

        if (!WaitForServer(port, 60))
        {
            MessageBox.Show(
                $"无法连接到服务 http://127.0.0.1:{port}，请确认 Shiny 已启动。",
                "LIS 数据看板",
                MessageBoxButtons.OK,
                MessageBoxIcon.Error);
            return 1;
        }

        // Configure CEF for unique process identity.
        // BrowserSubprocessPath set to our own exe means EVERY CEF subprocess
        // (renderer, GPU, etc.) is also launched as "dashboard_viewer.exe" -
        // zero process-name overlap with WhatsApp or any other app.
        var appData = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
            "LISDashboard");

        var settings = new CefSettings
        {
            BrowserSubprocessPath = Application.ExecutablePath,
            RootCachePath         = Path.Combine(appData, "CEF"),
            CachePath             = Path.Combine(appData, "CEF", "Cache"),
            LogFile               = Path.Combine(appData, "CEF", "cef_debug.log"),
            LogSeverity           = LogSeverity.Verbose
        };

        if (!Cef.Initialize(settings))
        {
            MessageBox.Show(
                "CEF 初始化失败。",
                "LIS 数据看板",
                MessageBoxButtons.OK,
                MessageBoxIcon.Error);
            return 1;
        }

        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);
        Application.Run(new MainForm(port));

        Cef.Shutdown();
        return 0;
    }

    private static bool WaitForServer(int port, int timeoutSecs)
    {
        // Use raw TCP to check if the port is open instead of HTTP.
        // HttpWebRequest initializes the .NET HTTP stack which interferes
        // with CEF/Chromium's own network stack initialization.
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
            catch
            {
                // Server not ready yet
            }
            Thread.Sleep(200);
        }
        return false;
    }
}
}
