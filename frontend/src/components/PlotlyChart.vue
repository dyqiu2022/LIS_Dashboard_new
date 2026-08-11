<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import Plotly from 'plotly.js-dist-min'

const props = defineProps<{ spec: any | null; height?: string }>()
const emit = defineEmits<{ click: [event: any] }>()
const el = ref<HTMLElement | null>(null)
const expandedEl = ref<HTMLElement | null>(null)
const expanded = ref(false)
let resizeObserver: ResizeObserver | null = null
let resizeFrame: number | null = null
const chartTitle = computed(() => {
  const title = props.spec?.layout?.title
  return typeof title === 'string' ? title : title?.text || 'LIS 数据图表'
})
const displaySpec = computed(() => normalizeSpec(props.spec))

function shortenLegendText(value: unknown, limit = 24) {
  const text = String(value ?? '')
  const chars = Array.from(text)
  return chars.length > limit ? `${chars.slice(0, limit - 1).join('')}…` : text
}

function normalizeSpec(spec: any | null) {
  if (!spec) return null
  const sourceData = Array.isArray(spec.data) ? spec.data : []
  const legendTexts: string[] = []
  const data = sourceData.map((trace: any) => {
    const next = { ...trace }
    if (next.showlegend !== false && next.name) legendTexts.push(String(next.name))
    if (next.type === 'pie' && Array.isArray(next.labels)) {
      const fullLabels = next.labels.map((label: unknown) => String(label ?? ''))
      legendTexts.push(...fullLabels)
      next.labels = fullLabels.map((label: string) => shortenLegendText(label))
      next.hovertext = fullLabels
      next.hoverinfo = 'text'
    } else if (next.name) {
      next.name = shortenLegendText(next.name)
    }
    return next
  })
  const layout = { ...(spec.layout || {}) }
  const hasLongLegend = legendTexts.some((text) => Array.from(text).length > 24)
  const legendCount = legendTexts.length
  if (layout.showlegend !== false && (hasLongLegend || legendCount > 8)) {
    const margin = { ...(layout.margin || {}) }
    layout.legend = {
      ...(layout.legend || {}),
      orientation: 'v',
      x: 1.01,
      y: 1,
      xanchor: 'left',
      yanchor: 'top',
      entrywidthmode: 'pixels',
      entrywidth: 140,
      itemwidth: 30,
      maxheight: 0.78,
      font: { ...(layout.legend?.font || {}), size: 9 },
    }
    margin.r = Math.max(Number(margin.r) || 0, 165)
    layout.margin = margin
  }
  return { ...spec, data, layout }
}

async function renderTarget(target: HTMLElement | null) {
  if (!target) return
  if (!displaySpec.value) {
    if ((target as any).data || (target as any)._fullLayout) await Plotly.purge(target)
    return
  }
  await Plotly.react(target, displaySpec.value.data || [], displaySpec.value.layout || {}, {
    responsive: true,
    displaylogo: false,
    modeBarButtonsToRemove: ['lasso2d', 'select2d'],
  })
  ;(target as any).removeAllListeners?.('plotly_click')
  ;(target as any).on?.('plotly_click', (event: any) => emit('click', event))
}

async function render() {
  await nextTick()
  await renderTarget(el.value)
  if (expanded.value) await renderTarget(expandedEl.value)
}

function resizeCharts() {
  if (el.value && (el.value as any).data) Plotly.Plots.resize(el.value)
  if (expandedEl.value && (expandedEl.value as any).data) Plotly.Plots.resize(expandedEl.value)
}

function onContainerResize() {
  if (resizeFrame !== null) return
  resizeFrame = window.requestAnimationFrame(() => {
    resizeFrame = null
    resizeCharts()
  })
}

function closeExpanded() {
  expanded.value = false
}

function serialized(value: unknown) {
  // Prevent user-provided labels from closing the generated script tag.
  return JSON.stringify(value ?? null).replace(/</g, '\\u003c')
}

function downloadHtml() {
  if (!props.spec) return
  const html = `<!doctype html>
<html lang="zh-CN">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>${chartTitle.value}</title>
  <script src="https://cdn.plot.ly/plotly-2.35.2.min.js"><\/script>
  <style>html,body,#plot{width:100%;height:100%;margin:0}body{overflow:hidden}</style>
</head>
<body>
  <div id="plot"></div>
  <script>
    const data = ${serialized(displaySpec.value?.data || [])};
    const layout = ${serialized(displaySpec.value?.layout || {})};
    const config = {responsive: true, displaylogo: false};
    Plotly.newPlot('plot', data, layout, config);
  <\/script>
</body>
</html>`
  const url = URL.createObjectURL(new Blob([html], { type: 'text/html;charset=utf-8' }))
  const link = document.createElement('a')
  link.href = url
  link.download = `${chartTitle.value.replace(/[\\/:*?"<>|]/g, '_') || 'LIS图表'}.html`
  link.click()
  URL.revokeObjectURL(url)
}

watch(() => props.spec, render, { deep: true })
watch(expanded, async (value) => {
  await nextTick()
  if (value) await renderTarget(expandedEl.value)
  else if (expandedEl.value) await Plotly.purge(expandedEl.value)
})
onMounted(render)
onMounted(() => {
  if (el.value) {
    resizeObserver = new ResizeObserver(onContainerResize)
    resizeObserver.observe(el.value)
  }
})
onBeforeUnmount(() => {
  resizeObserver?.disconnect()
  if (resizeFrame !== null) window.cancelAnimationFrame(resizeFrame)
  if (el.value) Plotly.purge(el.value)
  if (expandedEl.value) Plotly.purge(expandedEl.value)
})
</script>

<template>
  <div class="plotly-shell">
    <div v-if="spec" class="plotly-toolbar">
      <button type="button" class="plotly-tool-button" title="放大图表" @click="expanded = true">⛶ <span>放大</span></button>
      <button type="button" class="plotly-tool-button" title="下载交互式 HTML 图表" @click="downloadHtml">↓ <span>下载 HTML</span></button>
    </div>
    <div ref="el" class="plotly-host" :style="{ height: height || '420px' }">
      <div v-if="!spec" class="plotly-empty">
        <span class="plotly-empty-mark">⌁</span>
        <p>运行分析后将在这里展示结果</p>
      </div>
    </div>
    <div v-if="expanded" class="plotly-lightbox" role="dialog" aria-modal="true" :aria-label="`${chartTitle} 放大视图`">
      <div class="plotly-lightbox-header"><strong>{{ chartTitle }}</strong><div class="plotly-lightbox-actions"><button type="button" class="plotly-tool-button" @click="downloadHtml">↓ <span>下载 HTML</span></button><button type="button" class="plotly-tool-button plotly-close-button" title="关闭放大视图" @click="closeExpanded">× <span>关闭</span></button></div></div>
      <div ref="expandedEl" class="plotly-expanded-host" />
    </div>
  </div>
</template>
