using CefSharp;
using CefSharp.WinForms;

namespace LISDashboard
{

public class MainForm : Form
{
    private readonly ChromiumWebBrowser _browser;

    private const string ZoomScript = @"
(function() {
  function addZoom() {
    document.addEventListener('wheel', function(e) {
      if (e.ctrlKey) {
        e.preventDefault();
        var delta = e.deltaY > 0 ? -0.1 : 0.1;
        var cur = parseFloat(document.body.style.zoom) || 1;
        document.body.style.zoom = Math.max(0.5, Math.min(2, cur + delta));
      }
    }, { passive: false });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', addZoom);
  } else {
    addZoom();
  }
})();
";

    public MainForm(int port)
    {
        var url = $"http://127.0.0.1:{port}";

        Text = "LIS 数据看板";
        Size = new Size(1280, 800);
        StartPosition = FormStartPosition.CenterScreen;

        try
        {
            Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath);
        }
        catch
        {
            // ignore
        }

        _browser = new ChromiumWebBrowser(url)
        {
            Dock = DockStyle.Fill
        };

        _browser.FrameLoadEnd += OnFrameLoadEnd;
        Controls.Add(_browser);
    }

    private void OnFrameLoadEnd(object sender, FrameLoadEndEventArgs e)
    {
        if (e.Frame.IsMain)
        {
            _browser.ExecuteScriptAsync(ZoomScript);
        }
    }

    protected override void Dispose(bool disposing)
    {
        if (disposing)
        {
            _browser.Dispose();
        }
        base.Dispose(disposing);
    }
}
}
