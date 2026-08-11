import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

export default defineConfig({
  plugins: [vue()],
  server: {
    host: '0.0.0.0',
    port: 5173,
    proxy: {
      '/api': 'http://localhost:8000',
      '/healthz': 'http://localhost:8000',
    },
  },
  // Vite preview does not inherit the dev server proxy in every version.
  // Keep the WSL-native :18080 entrypoint usable with same-origin API calls.
  preview: {
    host: '0.0.0.0',
    port: 4173,
    proxy: {
      '/api': 'http://localhost:18000',
      '/healthz': 'http://localhost:18000',
    },
  },
})
