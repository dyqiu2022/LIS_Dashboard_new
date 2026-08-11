// Docker/Nginx serves the API under the same origin.  The WSL-native
// deployment uses Vite preview on :18080 without Vite's dev proxy, so point
// browser requests at the companion FastAPI process on :18000 in that mode.
export function defaultApiBase(location?: { protocol: string; hostname: string; port: string }): string {
  const current = location || (typeof window !== 'undefined' ? window.location : undefined)
  return current?.port === '18080'
    ? `${current.protocol}//${current.hostname}:18000`
    : ''
}
const API_BASE = (import.meta.env.VITE_API_BASE || defaultApiBase()).replace(/\/$/, '')

function resolvePath(path: string): string {
  return API_BASE && path.startsWith(`${API_BASE}/`) ? path : `${API_BASE}${path}`
}

export async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const response = await fetch(resolvePath(path), {
    ...init,
    headers: { 'Content-Type': 'application/json', ...(init.headers || {}) },
  })
  if (!response.ok) {
    let detail = response.statusText
    try {
      const body = await response.json()
      detail = body.detail || detail
    } catch (_) {
      // Keep the HTTP status when the server returned a non-JSON error page.
    }
    throw new Error(detail)
  }
  return response.json()
}

export function apiUrl(path: string): string {
  return resolvePath(path)
}

export async function upload(files: File[]): Promise<any> {
  const form = new FormData()
  files.forEach((file) => form.append('files', file))
  const response = await fetch(resolvePath('/api/datasets/upload'), { method: 'POST', body: form })
  if (!response.ok) {
    const body = await response.json().catch(() => ({}))
    throw new Error(body.detail || response.statusText)
  }
  return response.json()
}
