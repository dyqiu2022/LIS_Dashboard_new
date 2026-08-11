import { describe, expect, it } from 'vitest'
import { apiUrl, defaultApiBase } from './api'

describe('api URL', () => {
  it('keeps API paths relative for nginx deployment', () => {
    expect(apiUrl('/api/datasets')).toContain('/api/datasets')
  })

  it('uses the companion FastAPI port for WSL preview', () => {
    expect(defaultApiBase({ protocol: 'http:', hostname: 'localhost', port: '18080' })).toBe('http://localhost:18000')
    expect(defaultApiBase({ protocol: 'http:', hostname: 'localhost', port: '5173' })).toBe('')
  })
})
