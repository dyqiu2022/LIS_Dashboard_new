// LIS Dashboard 轻量级 GUI 启动窗口
// 使用系统 WebView（Windows=WebView2），支持 Ctrl+滚轮缩放
package main

import (
	"fmt"
	"net/http"
	"os"
	"strconv"
	"time"

	webview "github.com/webview/webview_go"
)

const (
	defaultPort = 8888
	appTitle    = "LIS 数据看板"
)

// 注入 Ctrl+滚轮 缩放的 JS
const zoomScript = `
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
`

func waitForServer(url string, timeout time.Duration) bool {
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		resp, err := http.Get(url)
		if err == nil {
			resp.Body.Close()
			if resp.StatusCode < 500 {
				return true
			}
		}
		time.Sleep(200 * time.Millisecond)
	}
	return false
}

func main() {
	port := defaultPort
	if len(os.Args) > 1 {
		if p, err := strconv.Atoi(os.Args[1]); err == nil && p > 0 && p < 65536 {
			port = p
		}
	}

	url := fmt.Sprintf("http://127.0.0.1:%d", port)
	if !waitForServer(url, 60*time.Second) {
		fmt.Fprintf(os.Stderr, "无法连接到服务 %s，请确认 Shiny 已启动。\n", url)
		os.Exit(1)
	}

	debug := false
	w := webview.New(debug)
	defer w.Destroy()

	w.SetTitle(appTitle)
	w.SetSize(1280, 800, webview.HintNone)
	w.Navigate(url)

	// 页面加载后注入 Ctrl+滚轮 缩放
	go func() {
		time.Sleep(1 * time.Second)
		w.Dispatch(func() {
			w.Eval(zoomScript)
		})
		time.Sleep(2 * time.Second)
		w.Dispatch(func() {
			w.Eval(zoomScript)
		})
	}()

	w.Run()
}
