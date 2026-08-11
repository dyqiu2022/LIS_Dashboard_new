<script setup lang="ts">
import { computed, onBeforeUnmount, reactive, ref } from 'vue'
import { ElMessage } from 'element-plus'
import PlotlyChart from './components/PlotlyChart.vue'
import PaginatedTable from './components/PaginatedTable.vue'
import CollapsibleSidebar from './components/CollapsibleSidebar.vue'
import { api, apiUrl, upload } from './api'
import type { DatasetInfo, PlotSpec } from './types'

const dataset = ref<DatasetInfo | null>(null)
const schema = ref<Array<Record<string, any>>>([])
const rows = ref<Record<string, any>[]>([])
const totalRows = ref(0)
const page = ref(1)
const pageSize = ref(100)
const activeTab = ref('analysis')
const clusterTab = ref('keyword')
const analysisTab = ref('data')
const batchChartTab = ref('line')
const batchDetailTab = ref('detail')
const loading = ref(false)
const status = ref('请上传 Excel 或 CSV 数据')
const selectedFiles = ref<File[]>([])
const sidebarSections = ref(['variables'])
const sidebarCollapsed = reactive({
  analysis: false,
  keyword: false,
  unsupervised: false,
  discretization: false,
  discretizationChart: false,
  qualitative: false,
  qualitativeTop: false,
  qualitativeDiscrete: false,
  qualitativeBottom: false,
  quantitative: false,
  batch: false,
})
const selectedFileNames = computed(() => selectedFiles.value.map((file) => file.name))

const filters = reactive({
  invalid_date: true,
  invalid_age: true,
  invalid_result: true,
  categorical: {} as Record<string, string[]>,
  ranges: {} as Record<string, Array<string | number | null>>,
})
const activeFilterCount = computed(() => {
  const categorical = Object.entries(filters.categorical).filter(([name, values]) => {
    const item = schema.value.find((schemaItem) => schemaItem.name === name)
    return item?.choices && values.length !== item.choices.length
  }).length
  const ranges = Object.entries(filters.ranges).filter(([name, range]) => {
    const item = schema.value.find((schemaItem) => schemaItem.name === name)
    if (!item) return false
    return String(range[0] ?? '') !== String(item.min ?? '') || String(range[1] ?? '') !== String(item.max ?? '')
  }).length
  const largeCategorical = Object.values(largeCategoricalValues).filter((value) => value.trim()).length
  const invalid = [filters.invalid_date, filters.invalid_age, filters.invalid_result].filter((value) => !value).length
  return categorical + ranges + largeCategorical + invalid
})

const numericSchema = computed(() => schema.value.filter((item) => item.dtype?.includes('Int') || item.dtype?.includes('Float')))
const dateSchema = computed(() => schema.value.filter((item) => item.dtype?.includes('Date') || item.name === '采样时间'))
const categoricalSchema = computed(() => schema.value.filter((item) => item.choices && item.choices.length > 1 && item.choices.length <= 200 && !numericSchema.value.includes(item) && !dateSchema.value.includes(item)))
const largeCategoricalSchema = computed(() => schema.value.filter((item) => item.dtype === 'String' && item.unique_count > 200 && !dateSchema.value.includes(item)))
const largeCategoricalValues = reactive<Record<string, string>>({})
const analysisColumns = computed(() => dataset.value?.columns || [])
const qualitativeColumns = computed(() => analysisColumns.value.filter((name) => !['采样时间', '项目序号', '病人ID', '检验单号', '年龄', '临床诊断', '参考区间', '定量结果', '原始结果', '调研人'].includes(name)))
const groupingColumns = computed(() => analysisColumns.value.filter((name) => !['类别_关键词', '类别_无监督'].includes(name)))
const embeddingModels = [
  { value: 'codefuse-ai/F2LLM-v2-4B', label: 'F2LLM-v2-4B（质量优先，约 7.6 GB）' },
  { value: 'codefuse-ai/F2LLM-v2-1.7B', label: 'F2LLM-v2-1.7B（速度优先，约 3.4 GB）' },
]
const invalidTotal = computed(() => Object.values(dataset.value?.invalid_counts || {}).reduce((total, count) => total + count, 0))
const qualityRate = computed(() => {
  if (!dataset.value?.row_count) return 100
  return Math.max(0, Math.round((1 - invalidTotal.value / dataset.value.row_count) * 100))
})
const activeWorkspaceTitle = computed(() => {
  if (activeTab.value === 'cluster') return '聚类工具'
  return ({ data: '数据概览', discretize: '数值变量离散化', qualitative: '探索性定性分析', quantitative: '探索性定量分析', batch: '批间差异分析' } as Record<string, string>)[analysisTab.value] || '分析工具'
})

function jobStatusText(value?: string) {
  return ({ running: '运行中', completed: '已完成', failed: '失败', pending: '排队中' } as Record<string, string>)[value || ''] || '处理中'
}

function setSelectedFiles(files: File[]) {
  selectedFiles.value = files.filter((file) => /\.(xlsx?|csv)$/i.test(file.name))
  if (selectedFiles.value.length) status.value = `已选择 ${selectedFiles.value.length} 个文件，等待上传`
}

function onDrop(event: DragEvent) {
  setSelectedFiles(Array.from(event.dataTransfer?.files || []))
}

function initFilters(info: DatasetInfo) {
  schema.value = info.schema
  for (const key of Object.keys(filters.categorical)) delete filters.categorical[key]
  for (const key of Object.keys(filters.ranges)) delete filters.ranges[key]
  for (const key of Object.keys(largeCategoricalValues)) delete largeCategoricalValues[key]
  for (const item of info.schema) {
    const isNumeric = item.dtype?.includes('Int') || item.dtype?.includes('Float')
    const isDate = item.dtype?.includes('Date') || item.name === '采样时间'
    if (item.choices?.length > 1 && item.choices.length <= 200 && !isNumeric && !isDate) {
      filters.categorical[item.name] = item.choices.map((value: any) => String(value))
    }
    if (item.min !== undefined && item.max !== undefined) {
      filters.ranges[item.name] = [item.min, item.max]
    }
  }
  const numericNames = info.schema.filter((item) => item.dtype?.includes('Int') || item.dtype?.includes('Float')).map((item) => item.name)
  if (!discretization.column || !numericNames.includes(discretization.column)) discretization.column = numericNames.includes('定量结果') ? '定量结果' : (numericNames[0] || '')
  const preferredGrouping = info.columns.includes('性别') ? '性别' : (info.columns[0] || '')
  if (!quantitative.grouping || !info.columns.includes(quantitative.grouping)) quantitative.grouping = preferredGrouping
  const preferredClusterGrouping = info.columns.includes('临床诊断') ? '临床诊断' : (info.columns[0] || '')
  if (!keyword.groupingCol || !info.columns.includes(keyword.groupingCol)) keyword.groupingCol = preferredClusterGrouping
  if (!unsupervised.groupingCol || !info.columns.includes(unsupervised.groupingCol)) unsupervised.groupingCol = preferredClusterGrouping
  const qualitativeCandidates = info.columns.filter((name) => !['采样时间', '项目序号', '病人ID', '检验单号', '年龄', '临床诊断', '参考区间', '定量结果', '原始结果', '调研人'].includes(name))
  const preferredPrimary = info.columns.includes('定性结果') ? '定性结果' : (qualitativeCandidates[0] || info.columns[0] || '')
  if (!qualitative.primary || !info.columns.includes(qualitative.primary)) qualitative.primary = preferredPrimary
  if (!qualitative.secondary || !info.columns.includes(qualitative.secondary)) qualitative.secondary = info.columns.includes('性别') && qualitative.primary !== '性别' ? '性别' : (info.columns.find((name) => name !== qualitative.primary) || '')
}

function filterPayload() {
  for (const item of largeCategoricalSchema.value) {
    filters.categorical[item.name] = (largeCategoricalValues[item.name] || '').split(',').map((value) => value.trim()).filter(Boolean)
  }
  return {
    invalid_date: filters.invalid_date,
    invalid_age: filters.invalid_age,
    invalid_result: filters.invalid_result,
    categorical: filters.categorical,
    ranges: filters.ranges,
  }
}

async function refreshRows() {
  if (!dataset.value) return
  loading.value = true
  try {
    const result = await api<any>(`/api/datasets/${dataset.value.dataset_id}/rows`, {
      method: 'POST',
      body: JSON.stringify({ ...filterPayload(), offset: (page.value - 1) * pageSize.value, limit: pageSize.value }),
    })
    rows.value = result.rows
    totalRows.value = result.total
  } catch (error: any) {
    ElMessage.error(error.message)
  } finally {
    loading.value = false
  }
}

async function doUpload() {
  if (!selectedFiles.value.length) return ElMessage.warning('请选择 Excel 或 CSV 文件')
  status.value = `正在处理 ${selectedFiles.value.length} 个文件…`
  loading.value = true
  status.value = '正在读取并规范化数据，请稍候…'
  try {
    const info = await upload(selectedFiles.value)
    dataset.value = info
    initFilters(info)
    page.value = 1
    await refreshRows()
    selectedFiles.value = []
    status.value = `已上传 ${info.row_count.toLocaleString()} 行，${info.columns.length} 列`
    ElMessage.success('数据上传完成')
  } catch (error: any) {
    status.value = '上传失败'
    ElMessage.error(error.message)
  } finally {
    loading.value = false
  }
}

function onFiles(event: Event) {
  const input = event.target as HTMLInputElement
  setSelectedFiles(Array.from(input.files || []))
}

function applyFilters() {
  for (const item of largeCategoricalSchema.value) {
    filters.categorical[item.name] = (largeCategoricalValues[item.name] || '').split(',').map((value) => value.trim()).filter(Boolean)
  }
  page.value = 1
  refreshRows()
}

function clearFilters() {
  for (const item of categoricalSchema.value) filters.categorical[item.name] = item.choices.map((value: any) => String(value))
  for (const item of largeCategoricalSchema.value) {
    largeCategoricalValues[item.name] = ''
    filters.categorical[item.name] = []
  }
  for (const item of numericSchema.value) filters.ranges[item.name] = [item.min, item.max]
  for (const item of dateSchema.value) filters.ranges[item.name] = [item.min, item.max]
  filters.invalid_date = true
  filters.invalid_age = true
  filters.invalid_result = true
  applyFilters()
}

function changePage(value: number) {
  page.value = value
  refreshRows()
}

function changePageSize(value: number) {
  pageSize.value = value
  page.value = 1
  refreshRows()
}

async function downloadCsv() {
  if (!dataset.value) return
  const response = await fetch(apiUrl(`/api/datasets/${dataset.value.dataset_id}/download`), {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ ...filterPayload(), offset: 0, limit: 5000 }),
  })
  if (!response.ok) return ElMessage.error('下载失败')
  const blob = await response.blob()
  const link = document.createElement('a')
  link.href = URL.createObjectURL(blob)
  link.download = `完整筛选数据_${new Date().toISOString().slice(0, 10)}.csv`
  link.click()
  URL.revokeObjectURL(link.href)
}

const keyword = reactive({
  groupingCol: '',
  className: '',
  and1: '',
  and2: '',
  and3: '',
  notWords: '',
  notLimit: '',
  excludeGroups: [] as string[],
  sentence: '',
  matched: 0,
  plot: null as PlotSpec | null,
  rows: [] as any[],
  definitions: [] as any[],
})
const keywordDefinition = computed(() => ({
  class_name: keyword.className,
  and1: keyword.and1,
  and2: keyword.and2,
  and3: keyword.and3,
  not_words: keyword.notWords,
  not_limit: keyword.notLimit,
  exclude_groups: keyword.excludeGroups,
}))
const keywordExcludeOptions = computed(() => keyword.definitions.map((definition: any) => definition.class_name))

async function keywordPreview() {
  if (!dataset.value || !keyword.groupingCol || !keyword.className) return ElMessage.warning('请选择分组列并填写类别名称')
  try {
    const result = await api<any>(`/api/clustering/${dataset.value.dataset_id}/keyword/preview`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), grouping_col: keyword.groupingCol, definition: keywordDefinition.value }),
    })
    keyword.sentence = result.sentence
    keyword.matched = result.matched_count
    keyword.plot = result.plot || null
    const counts = await api<any>(`/api/clustering/${dataset.value.dataset_id}/keyword/counts`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), grouping_col: keyword.groupingCol, definition: keywordDefinition.value }),
    })
    keyword.rows = counts.rows
  } catch (error: any) { ElMessage.error(error.message) }
}

async function keywordWrite() {
  if (!dataset.value || !keyword.className) return
  try {
    const result = await api<any>(`/api/clustering/${dataset.value.dataset_id}/keyword/write`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), grouping_col: keyword.groupingCol, definitions: [...keyword.definitions, keywordDefinition.value] }),
    })
    keyword.definitions.push(keywordDefinition.value)
    dataset.value = result.dataset
    initFilters(result.dataset)
    await refreshRows()
    ElMessage.success('关键词类别已写入数据')
  } catch (error: any) { ElMessage.error(error.message) }
}

const unsupervised = reactive({ groupingCol: '', clusterNum: 30, modelName: embeddingModels[0].value, instruction: '对临床诊断进行分类，生成适合聚类且有代表性的词向量', job: null as any, rows: [] as any[] })
let pollTimer: number | undefined
async function pollJob(jobId: string, onUpdate: (job: any) => void) {
  if (pollTimer) window.clearInterval(pollTimer)
  let finished = false
  const poll = async () => {
    try {
      const job = await api<any>(`/api/jobs/${jobId}`)
      onUpdate(job)
      if (job.status === 'completed' || job.status === 'failed') {
        finished = true
        if (pollTimer) window.clearInterval(pollTimer)
      }
    } catch (_) { if (pollTimer) window.clearInterval(pollTimer) }
  }
  await poll()
  if (!finished) pollTimer = window.setInterval(poll, 1200)
}

async function writeUnsupervised() {
  if (!dataset.value || !unsupervised.job?.job_id || !unsupervised.groupingCol) return
  try {
    const result = await api<any>(`/api/clustering/${dataset.value.dataset_id}/unsupervised/${unsupervised.job.job_id}/write?grouping_col=${encodeURIComponent(unsupervised.groupingCol)}`, { method: 'POST' })
    dataset.value = result.dataset
    initFilters(result.dataset)
    await refreshRows()
    ElMessage.success('无监督聚类结果已写入数据')
  } catch (error: any) { ElMessage.error(error.message) }
}

async function startUnsupervised() {
  if (!dataset.value || !unsupervised.groupingCol) return ElMessage.warning('请选择聚类列')
  try {
    const job = await api<any>(`/api/clustering/${dataset.value.dataset_id}/unsupervised`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), grouping_col: unsupervised.groupingCol, cluster_num: unsupervised.clusterNum, model_name: unsupervised.modelName, instruction: unsupervised.instruction.trim() || null }),
    })
    unsupervised.job = job
    await pollJob(job.job_id, (result) => {
      unsupervised.job = result
      if (result.status === 'completed') unsupervised.rows = result.result.rows
      else if (result.status === 'failed') ElMessage.error(result.error || '聚类失败')
    })
  } catch (error: any) { ElMessage.error(error.message) }
}

const discretization = reactive({
  column: '', cutPoints: '5%|30%|50%|70%|95%', transform: '原数据', bins: 50,
  spec: null as PlotSpec | null, discretizedSpec: null as PlotSpec | null,
  distributionRows: [] as any[], groupRows: [] as any[], message: '', lambda: null as number | null,
})
async function previewDiscretization() {
  if (!dataset.value || !discretization.column) return ElMessage.warning('请选择数值变量')
  try {
    const result = await api<any>(`/api/analysis/${dataset.value.dataset_id}/histogram`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), column: discretization.column, cut_points: discretization.cutPoints, transform: discretization.transform, bins: discretization.bins }),
    })
    discretization.spec = result.plot
    discretization.discretizedSpec = result.discretized_plot || null
    discretization.distributionRows = result.distribution_table || []
    discretization.groupRows = result.group_table || []
    discretization.lambda = result.plot?.lambda ?? null
    discretization.message = result.cut_info.description
  } catch (error: any) { ElMessage.error(error.message) }
}
async function applyDiscretization() {
  if (!dataset.value || !discretization.column) return ElMessage.warning('请选择数值变量')
  try {
    const result = await api<any>(`/api/analysis/${dataset.value.dataset_id}/discretize`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), column: discretization.column, cut_points: discretization.cutPoints }),
    })
    dataset.value = result.dataset
    initFilters(result.dataset)
    await refreshRows()
    ElMessage.success(`已生成 ${result.derived_column}`)
  } catch (error: any) { ElMessage.error(error.message) }
}

const qualitative = reactive({
  primary: '', secondary: '', colorMode: '默认颜色', orderDirection: 1, elements: 20, discreteYMode: '数量',
  topXVar: '采样时间', topYMode: '数量', topGrain: 'month', topNormalize: false,
  bottomXVar: '年龄', bottomYMode: '数量', bottomGrain: '3', bottomNormalize: false,
  pie: null as PlotSpec | null, discrete: null as PlotSpec | null,
  consecutiveTop: null as PlotSpec | null, consecutiveBottom: null as PlotSpec | null,
})
async function loadQualitative() {
  if (!dataset.value || !qualitative.primary) return ElMessage.warning('请选择一级分层变量')
  try {
    const base = { ...filterPayload(), primary_col: qualitative.primary, color_mode: qualitative.colorMode, elements_num: qualitative.elements, order_direction: qualitative.orderDirection }
    qualitative.pie = await api<PlotSpec>(`/api/analysis/${dataset.value.dataset_id}/pie`, { method: 'POST', body: JSON.stringify(base) })
    if (qualitative.secondary && qualitative.secondary !== qualitative.primary) {
      qualitative.discrete = await api<PlotSpec>(`/api/analysis/${dataset.value.dataset_id}/discrete-stack`, { method: 'POST', body: JSON.stringify({ ...base, secondary_col: qualitative.secondary, y_mode: qualitative.discreteYMode }) })
    } else qualitative.discrete = null
    qualitative.consecutiveTop = await api<PlotSpec>(`/api/analysis/${dataset.value.dataset_id}/consecutive-stack`, { method: 'POST', body: JSON.stringify({ ...base, x_var: qualitative.topXVar, y_mode: qualitative.topYMode, grain: qualitative.topGrain, normalize_quantitative: qualitative.topNormalize }) })
    qualitative.consecutiveBottom = await api<PlotSpec>(`/api/analysis/${dataset.value.dataset_id}/consecutive-stack`, { method: 'POST', body: JSON.stringify({ ...base, x_var: qualitative.bottomXVar, y_mode: qualitative.bottomYMode, grain: qualitative.bottomGrain, normalize_quantitative: qualitative.bottomNormalize }) })
  } catch (error: any) { ElMessage.error(error.message) }
}
function changeQualitativeOrder() { qualitative.orderDirection *= -1 }

const quantitative = reactive({ grouping: '', hoverMode: 'x unified', ci: ['95%', '80%', '50%', '20%', '5%'], smoothing: 1.3, winWidth: 5, minNum: 20, spec: null as PlotSpec | null })
async function loadQuantitative() {
  if (!dataset.value || !quantitative.grouping) return ElMessage.warning('请选择分层变量')
  try {
    quantitative.spec = await api<PlotSpec>(`/api/analysis/${dataset.value.dataset_id}/quantitative-trend`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), grouping_col: quantitative.grouping, ci: quantitative.ci, hover_mode: quantitative.hoverMode, smoothing: quantitative.smoothing, win_width: quantitative.winWidth, min_num: quantitative.minNum }),
    })
  } catch (error: any) { ElMessage.error(error.message) }
}

const batch = reactive({
  formula: '性别:I(年龄^2) + 性别:年龄 + 类别_无监督', n: 100, step: 100,
  job: null as any, rows: [] as any[], statsSummary: [] as any[], interpretation: '',
  formulaValid: false, formulaMessage: '', detail: null as any, dialogVisible: false,
})
const batchMethodVisible = ref(false)
function batchColor(p: number) { return p <= 0.01 ? '#e74c3c' : p <= 0.05 ? '#f39c12' : '#36a269' }
function validateBatchFormula() {
  if (!dataset.value || !batch.formula.trim()) return
  api<any>(`/api/batch-difference/${dataset.value.dataset_id}/validate`, {
    method: 'POST', body: JSON.stringify({ ...filterPayload(), formula: batch.formula, n_value: batch.n, step_value: batch.step }),
  }).then((result) => { batch.formulaValid = result.valid; batch.formulaMessage = result.message }).catch((error: any) => { batch.formulaValid = false; batch.formulaMessage = error.message })
}
const batchLineSpec = computed<PlotSpec | null>(() => {
  if (!batch.rows.length) return null
  const summary = new Map(batch.statsSummary.map((row: any) => [`${row.manu_name}__${row.quantile_level}`, row]))
  const keys = [...new Set(batch.rows.map((row: any) => `${row.manu_name}__${row.quantile_level}`))]
  const data: any[] = []
  for (const key of keys) {
    const part = batch.rows.filter((row: any) => `${row.manu_name}__${row.quantile_level}` === key)
    const stat = summary.get(key) || {}
    const label = `${key.replace('__', ' - ')} (IQR: ${Number(stat.IQR_val || 0).toFixed(2)}, Range: ${Number(stat.Range_val || 0).toFixed(2)}, Error Rate: ${Number(stat.error_rate || 0).toFixed(3)})`
    data.push({ type: 'scatter', mode: 'markers', name: key.replace('__', ' - '), showlegend: false, x: part.map((row: any) => row.mid_point), y: part.map((row: any) => row['等效水平']), customdata: part.map((row: any) => row.point_id), text: part.map((row: any) => row.hover_text), hoverinfo: 'text', marker: { size: 7, opacity: 0.75, color: part.map((row: any) => batchColor(Number(row.p_value_bonferroni))) }, legendgroup: key })
    data.push({ type: 'scatter', mode: 'lines', name: label, showlegend: true, x: part.map((row: any) => row.mid_point), y: part.map((row: any) => row['等效水平']), customdata: part.map((row: any) => row.point_id), hoverinfo: 'none', line: { color: 'rgba(0, 0, 255, .5)', width: 2 }, legendgroup: key })
  }
  return { data, layout: { title: '多厂家多分位数等效水平分布', xaxis: { title: '窗口中心位置' }, yaxis: { title: '等效水平' }, hoverlabel: { bgcolor: 'white', font: { color: 'black' } }, legend: { orientation: 'h', y: -0.2 } } }
})
const batchViolinSpec = computed<PlotSpec | null>(() => {
  if (!batch.rows.length) return null
  const levels = ['10%', '30%', '50%', '70%', '90%'].filter((level) => batch.rows.some((row: any) => row.quantile_level === level))
  const columns = levels.length <= 3 ? levels.length : 2
  const changes = batch.rows.map((row: any) => Number(row['等效波动'])).filter((value: number) => Number.isFinite(value))
  const mean = changes.reduce((sum, value) => sum + value, 0) / Math.max(changes.length, 1)
  const sd = Math.sqrt(changes.reduce((sum, value) => sum + (value - mean) ** 2, 0) / Math.max(changes.length - 1, 1))
  const bandwidth = 1.3 * sd * Math.pow(Math.max(changes.length, 1), -0.2)
  const data = levels.map((level, index) => {
    const rows = batch.rows.filter((row: any) => row.quantile_level === level)
    const axis = index === 0 ? '' : String(index + 1)
    return { type: 'violin', name: level, x: rows.map((row: any) => row.manu_name), y: rows.map((row: any) => row['等效波动']), customdata: rows.map((row: any) => row.point_id), text: rows.map((row: any) => row.hover_text), hoverinfo: 'text', box: { visible: true }, meanline: { visible: true }, points: 'all', pointpos: -1.5, jitter: 0.1, scalemode: 'width', bandwidth: bandwidth || undefined, showlegend: false, xaxis: `x${axis}`, yaxis: `y${axis}` }
  })
  return { data, layout: { title: '多厂家各分位数等效波动分布', grid: { rows: Math.ceil(levels.length / columns), columns, pattern: 'independent' }, yaxis: { title: '等效波动' }, margin: { b: 100 } } }
})
async function startBatch() {
  if (!dataset.value) return
  validateBatchFormula()
  try {
    const validation = await api<any>(`/api/batch-difference/${dataset.value.dataset_id}/validate`, { method: 'POST', body: JSON.stringify({ ...filterPayload(), formula: batch.formula, n_value: batch.n, step_value: batch.step }) })
    batch.formulaValid = validation.valid
    batch.formulaMessage = validation.message
    if (!validation.valid) return ElMessage.warning(validation.message)
    const job = await api<any>(`/api/batch-difference/${dataset.value.dataset_id}/run`, {
      method: 'POST', body: JSON.stringify({ ...filterPayload(), formula: batch.formula, n_value: batch.n, step_value: batch.step }),
    })
    batch.job = job
    batch.rows = []
    await pollJob(job.job_id, (result) => {
      batch.job = result
      if (result.status === 'completed') {
        batch.rows = result.result?.all_manu_data || []
        batch.statsSummary = result.result?.stats_summary || []
      } else if (result.status === 'failed') ElMessage.error(result.error || '批间差异分析失败')
    })
  } catch (error: any) { ElMessage.error(error.message) }
}
async function inspectBatch(row: any) {
  const pointId = row?.point_id || row?.customdata || row?.points?.[0]?.customdata
  if (!batch.job?.job_id || !pointId) return
  try {
    const result = await api<any>(`/api/batch-difference/jobs/${batch.job.job_id}/point/${encodeURIComponent(Array.isArray(pointId) ? pointId[0] : pointId)}`)
    batch.interpretation = result.interpretation
    batch.detail = result
    batchDetailTab.value = 'detail'
    batch.dialogVisible = true
  } catch (error: any) { ElMessage.error(error.message) }
}
function inspectBatchPlot(event: any) {
  const pointId = event?.points?.[0]?.customdata
  if (pointId) inspectBatch({ point_id: Array.isArray(pointId) ? pointId[0] : pointId })
}

onBeforeUnmount(() => { if (pollTimer) window.clearInterval(pollTimer) })
</script>

<template>
  <div class="app-shell">
    <header class="app-header">
      <div class="brand-block">
        <div class="brand-mark" aria-label="LIS"><span>LIS</span><i /></div>
        <div>
          <h1 class="app-title">LIS 数据看板</h1>
          <p class="app-subtitle">实验室数据探索与质量分析</p>
        </div>
      </div>
      <div class="header-right">
        <div v-if="dataset" class="header-overview" :title="`${activeWorkspaceTitle} · ${dataset.row_count.toLocaleString()} 行 · ${dataset.columns.length} 列`">
          <strong>{{ activeWorkspaceTitle }}</strong><span>{{ dataset.row_count.toLocaleString() }} 行</span><span>{{ dataset.columns.length }} 列</span><span>可见 {{ totalRows.toLocaleString() }}</span><span class="header-quality">质量 {{ qualityRate }}%</span>
        </div>
        <div class="header-upload">
           <input id="dataset-upload" class="visually-hidden" type="file" accept=".xlsx,.xls,.csv,text/csv" multiple @change="onFiles" />
          <label class="header-upload-trigger" for="dataset-upload" @dragover.prevent @drop.prevent="onDrop"><span class="header-upload-icon">↑</span><span>上传数据</span></label>
          <span v-if="selectedFiles.length" class="header-upload-pending" :title="selectedFileNames.join('、')">{{ selectedFiles.length }} 个文件待导入</span>
          <button v-if="selectedFiles.length" type="button" class="header-upload-clear" aria-label="清除待上传文件" @click="selectedFiles = []">×</button>
          <button v-if="selectedFiles.length" type="button" class="action-button action-button-primary action-button-small" :class="{ 'is-loading': loading }" :disabled="loading" @click="doUpload"><span v-if="loading" class="button-spinner" />{{ loading ? '导入中…' : '开始导入' }}</button>
        </div>
        <div v-if="dataset" class="header-dataset">
          <span class="status-dot ready" />
          <span class="header-dataset-name" :title="dataset.name">{{ dataset.name }}</span>
        </div>
        <span v-else class="status-text"><span class="status-dot" />{{ status }}</span>
        <span v-if="dataset" class="status-text"><span class="status-dot ready" />{{ status }}</span>
      </div>
    </header>

    <div class="app-body">
      <main class="app-content">
         <div v-if="!dataset" class="content-inner empty-workspace">
           <section class="welcome-card">
              <div class="welcome-copy">
                <span class="eyebrow">空白工作区</span>
                <h2>导入数据后开始分析</h2>
                <p>支持 Excel 和 CSV 文件。请使用页面右上角的上传入口。</p>
              </div>
              <div class="welcome-guide">
                <span class="eyebrow">开始使用</span>
                <div class="welcome-guide-item"><span>1</span><div><b>选择文件</b><small>支持 .xlsx / .xls / .csv</small></div></div>
                <div class="welcome-guide-item"><span>2</span><div><b>确认导入</b><small>完成后即可分析</small></div></div>
              </div>
           </section>
         </div>

        <div v-else class="content-inner">
          <el-tabs v-model="activeTab" class="workspace-tabs">
            <el-tab-pane name="cluster">
              <template #label><span class="tab-label"><i>01</i>聚类工具</span></template>
              <el-tabs v-model="clusterTab" class="module-tabs cluster-tabs">
                <el-tab-pane name="keyword">
                  <template #label>关键词聚类</template>
                  <div class="module-layout cluster-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.keyword }">
                    <CollapsibleSidebar v-model="sidebarCollapsed.keyword" class="module-control-sidebar" title="聚类参数与条件输入" eyebrow="RULE BASED" icon="A">
                      <p class="module-help">先定义规则并预览命中结果，再写入数据。控件只影响右侧聚类结果。</p>
                      <div class="module-control-list">
                        <div class="control-field"><span class="control-label">关键词聚类列</span><el-select v-model="keyword.groupingCol" filterable placeholder="选择用于匹配的字段"><el-option v-for="column in groupingColumns" :key="column" :label="column" :value="column" /></el-select></div>
                        <div class="control-field"><span class="control-label">新类别名称</span><el-input v-model="keyword.className" placeholder="例如：感染相关" /></div>
                        <div class="control-field"><span class="control-label">且条件 1 <em>可选</em></span><el-input v-model="keyword.and1" placeholder="关键词，使用 | 分隔" /></div>
                        <div class="control-field"><span class="control-label">且条件 2 <em>可选</em></span><el-input v-model="keyword.and2" placeholder="关键词，使用 | 分隔" /></div>
                        <div class="control-field"><span class="control-label">且条件 3 <em>可选</em></span><el-input v-model="keyword.and3" placeholder="关键词，使用 | 分隔" /></div>
                        <div class="control-field"><span class="control-label">排除关键词 <em>可选</em></span><el-input v-model="keyword.notWords" placeholder="不应命中的词" /></div>
                        <div class="control-field"><span class="control-label">排除限制 <em>可选</em></span><el-input v-model="keyword.notLimit" placeholder="限制条件" /></div>
                        <div class="control-field"><span class="control-label">互斥类别 <em>可选</em></span><el-select v-model="keyword.excludeGroups" multiple collapse-tags clearable placeholder="选择需要排除的类别"><el-option v-for="name in keywordExcludeOptions" :key="name" :label="name" :value="name" /></el-select></div>
                      </div>
                      <div class="module-actions"><button type="button" class="action-button" @click="keywordPreview">预览规则</button><button type="button" class="action-button action-button-primary" :disabled="!keyword.sentence" @click="keywordWrite">写入类别</button></div>
                      <div v-if="keyword.sentence" class="result-banner compact-banner"><span class="result-icon">✓</span><div><strong>{{ keyword.sentence }}</strong><p>命中 <b>{{ keyword.matched }}</b> 条记录</p></div></div>
                      <div v-if="keyword.definitions.length" class="definition-list"><span>待写入：</span><el-tag v-for="definition in keyword.definitions" :key="definition.class_name" size="small" effect="plain">{{ definition.class_name }}</el-tag></div>
                    </CollapsibleSidebar>
                    <div class="module-main">
                      <div class="tool-heading module-heading"><div class="tool-heading-index">A</div><div><span class="section-kicker">RULE BASED</span><h3>关键词聚类结果</h3><p>左侧规则与右侧表格、分布图保持绑定。</p></div><el-tag type="info" effect="plain">可解释</el-tag></div>
                      <div class="result-grid keyword-results"><div class="result-table"><div class="subpanel-heading"><strong>聚类统计表格</strong><span v-if="keyword.rows.length">{{ keyword.rows.length }} 个分组</span></div><PaginatedTable v-if="keyword.rows.length" :rows="keyword.rows" :columns="Object.keys(keyword.rows[0])" height="430" record-label="个分组" /><div v-else class="inline-empty">预览规则后显示分组结果</div></div><div class="result-chart"><div class="subpanel-heading"><strong>可视化结果</strong><span>与当前规则绑定</span></div><PlotlyChart :spec="keyword.plot" height="430px" /></div></div>
                    </div>
                  </div>
                </el-tab-pane>
                <el-tab-pane name="unsupervised">
                  <template #label>词向量无监督聚类</template>
                  <div class="module-layout cluster-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.unsupervised }">
                    <CollapsibleSidebar v-model="sidebarCollapsed.unsupervised" class="module-control-sidebar" title="无监督聚类参数" eyebrow="SEMANTIC MODEL" icon="B" tone="violet">
                      <p class="module-help">模型、目标簇数和提示词只作用于本次异步聚类任务。</p>
                      <div class="module-control-list">
                        <div class="control-field"><span class="control-label">聚类字段</span><el-select v-model="unsupervised.groupingCol" filterable placeholder="选择需要进行语义聚类的字段"><el-option v-for="column in groupingColumns" :key="column" :label="column" :value="column" /></el-select></div>
                        <div class="control-field"><span class="control-label">目标簇数量</span><el-input-number v-model="unsupervised.clusterNum" controls-position="right" :min="2" :max="500" /></div>
                        <div class="control-field"><span class="control-label">嵌入模型</span><el-select v-model="unsupervised.modelName" filterable><el-option v-for="model in embeddingModels" :key="model.value" :label="model.label" :value="model.value" /></el-select></div>
                        <div class="control-field"><span class="control-label">模型提示词 <em>可编辑</em></span><el-input v-model="unsupervised.instruction" type="textarea" :rows="4" placeholder="对临床诊断进行分类，生成适合聚类且有代表性的词向量" /></div>
                      </div>
                      <div class="module-actions module-actions-stack"><button type="button" class="action-button action-button-primary" :class="{ 'is-loading': unsupervised.job?.status === 'running' }" :disabled="unsupervised.job?.status === 'running'" @click="startUnsupervised"><span v-if="unsupervised.job?.status === 'running'" class="button-spinner" />{{ unsupervised.job?.status === 'running' ? '计算中…' : '开始语义聚类' }}</button><button v-if="unsupervised.job?.status === 'completed'" type="button" class="action-button action-button-secondary" @click="writeUnsupervised">写入聚类结果</button></div>
                      <div v-if="unsupervised.job" class="job-state"><div><span class="job-status-badge" :class="`job-status-${unsupervised.job.status}`">{{ jobStatusText(unsupervised.job.status) }}</span><span>{{ unsupervised.job.detail || '正在处理任务…' }}</span></div><div v-if="unsupervised.job.status !== 'completed'" class="task-progress"><span :style="{ width: `${Math.round((unsupervised.job.progress || 0) * 100)}%` }" /></div><small v-if="unsupervised.job.status !== 'completed'">{{ Math.round((unsupervised.job.progress || 0) * 100) }}%</small></div>
                    </CollapsibleSidebar>
                    <div class="module-main">
                      <div class="tool-heading module-heading"><div class="tool-heading-index violet">B</div><div><span class="section-kicker">SEMANTIC MODEL</span><h3>当前无监督聚类情况</h3><p>结果表按页查看，图表和写入仍使用完整聚类结果。</p></div><el-tag type="warning" effect="plain">异步任务</el-tag></div>
                      <div class="result-table result-table-wide"><div class="subpanel-heading"><strong>聚类结果</strong><span v-if="unsupervised.rows.length">{{ unsupervised.rows.length }} 条</span></div><PaginatedTable v-if="unsupervised.rows.length" :rows="unsupervised.rows" :columns="Object.keys(unsupervised.rows[0])" height="560" record-label="条聚类结果" /><div v-else class="inline-empty">完成一次聚类任务后显示结果表。</div></div>
                    </div>
                  </div>
                </el-tab-pane>
              </el-tabs>
            </el-tab-pane>

            <el-tab-pane name="analysis">
              <template #label><span class="tab-label"><i>02</i>分析工具</span></template>
              <div class="analysis-workspace" :class="{ 'sidebar-collapsed': sidebarCollapsed.analysis }">
                <CollapsibleSidebar v-model="sidebarCollapsed.analysis" class="analysis-sidebar" title="分析筛选条件" eyebrow="GLOBAL FILTER" icon="⌁">
                  <p class="module-help">这里的质量规则和变量筛选会同步作用于当前数据及所有分析模块。</p>
                  <el-collapse v-model="sidebarSections" class="filter-collapse">
                    <el-collapse-item name="quality">
                      <template #title>
                        <div class="collapse-title"><span class="collapse-icon">✓</span><span>质量规则</span><el-tag size="small" effect="plain">{{ activeFilterCount }} 项已调整</el-tag></div>
                      </template>
                      <div class="filter-section-content">
                        <p class="filter-help">关闭规则后，对应的无效记录会保留在结果中。</p>
                        <div class="checkbox-list">
                          <el-checkbox v-model="filters.invalid_date">过滤无效日期</el-checkbox>
                          <el-checkbox v-model="filters.invalid_age">过滤无效年龄</el-checkbox>
                          <el-checkbox v-model="filters.invalid_result">过滤无效定量结果</el-checkbox>
                        </div>
                      </div>
                    </el-collapse-item>
                    <el-collapse-item name="variables">
                      <template #title>
                        <div class="collapse-title"><span class="collapse-icon">⌁</span><span>变量筛选</span><el-tag v-if="activeFilterCount" size="small" type="warning" effect="plain">{{ activeFilterCount }}</el-tag></div>
                      </template>
                      <div class="filter-section-content">
                        <p class="filter-help">选择值或输入范围后，点击底部按钮更新数据视图。</p>
                        <div v-if="categoricalSchema.length || largeCategoricalSchema.length || numericSchema.length || dateSchema.length" class="filter-fields">
                          <div v-for="item in categoricalSchema" :key="item.name" class="filter-field">
                            <div class="field-label"><span>{{ item.name }}</span><em>{{ item.choices.length }} 项</em></div>
                            <el-select v-model="filters.categorical[item.name]" multiple collapse-tags filterable size="small" placeholder="选择保留值"><el-option v-for="value in item.choices" :key="String(value)" :label="String(value)" :value="String(value)" /></el-select>
                          </div>
                          <div v-for="item in largeCategoricalSchema" :key="`large-${item.name}`" class="filter-field">
                            <div class="field-label"><span>{{ item.name }}</span><em>精确匹配</em></div>
                            <el-input v-model="largeCategoricalValues[item.name]" size="small" :placeholder="`输入值，逗号分隔`" clearable />
                          </div>
                          <div v-for="item in numericSchema" :key="`range-${item.name}`" class="filter-field">
                            <div class="field-label"><span>{{ item.name }}</span><em>数值范围</em></div>
                            <div class="filter-range"><el-input v-model="filters.ranges[item.name][0]" size="small" :placeholder="String(item.min ?? '最小')" /><span>—</span><el-input v-model="filters.ranges[item.name][1]" size="small" :placeholder="String(item.max ?? '最大')" /></div>
                          </div>
                          <div v-for="item in dateSchema" :key="`date-${item.name}`" class="filter-field">
                            <div class="field-label"><span>{{ item.name }}</span><em>日期范围</em></div>
                            <div class="filter-range"><el-input v-model="filters.ranges[item.name][0]" size="small" placeholder="起始日期" /><span>—</span><el-input v-model="filters.ranges[item.name][1]" size="small" placeholder="结束日期" /></div>
                          </div>
                        </div>
                        <div v-else class="filter-empty">当前数据没有可配置的筛选字段。</div>
                        <div class="filter-actions"><button type="button" class="action-button action-button-primary" @click="applyFilters">应用筛选</button><button type="button" class="action-button" @click="clearFilters">重置</button></div>
                      </div>
                    </el-collapse-item>
                  </el-collapse>
                </CollapsibleSidebar>
                <div class="analysis-content">
                  <el-tabs v-model="analysisTab" class="module-tabs analysis-tabs">
                    <el-tab-pane name="data">
                      <template #label>当前数据</template>
                      <section class="content-panel data-panel">
                        <div class="panel-header">
                          <div><span class="section-kicker">CURRENT DATA</span><h3>数据明细表</h3><p>按页查看当前筛选结果；这里的筛选条件也会传递给其他分析模块。</p></div>
                          <div class="action-row"><button type="button" class="action-button action-button-small" @click="refreshRows">刷新数据</button><button type="button" class="action-button action-button-small action-button-primary" @click="downloadCsv">下载 CSV</button></div>
                        </div>
                        <div class="table-caption"><div><strong>{{ totalRows.toLocaleString() }} 条记录</strong><span class="caption-status"><i />已同步</span></div><span>每页 {{ pageSize }} 条 · 共 {{ dataset.columns.length }} 个字段</span></div>
                        <el-table class="data-table" :data="rows" stripe height="620" v-loading="loading">
                          <el-table-column v-for="column in dataset.columns" :key="column" :prop="column" :label="column" min-width="130" show-overflow-tooltip />
                        </el-table>
                        <div class="table-footer"><span>第 {{ page }} 页</span><el-pagination v-model:current-page="page" v-model:page-size="pageSize" background layout="total, sizes, prev, pager, next" :total="totalRows" @current-change="changePage" @size-change="changePageSize" /></div>
                      </section>
                    </el-tab-pane>

                    <el-tab-pane name="discretize">
                      <template #label>数值变量离散化</template>
                      <div class="module-layout analysis-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.discretization }">
                        <CollapsibleSidebar v-model="sidebarCollapsed.discretization" class="module-control-sidebar" title="离散化参数" eyebrow="TRANSFORM" icon="C" tone="amber">
                          <p class="module-help">先选择变量和切点生成预览；确认后再覆盖写入派生字段。</p>
                          <div class="module-control-list">
                            <div class="control-field"><span class="control-label">离散化变量</span><el-select v-model="discretization.column" filterable placeholder="选择数值变量"><el-option v-for="column in numericSchema.map(x => x.name)" :key="column" :label="column" :value="column" /></el-select></div>
                            <div class="control-field"><span class="control-label">输入分割点 <em>使用 | 分隔</em></span><el-input v-model="discretization.cutPoints" placeholder="20|50|80 或 5%|30%|60%" /></div>
                          </div>
                          <div class="module-actions module-actions-stack"><button type="button" class="action-button action-button-primary" @click="previewDiscretization">确认分割并预览</button><button type="button" class="action-button" @click="previewDiscretization">刷新预览</button><button type="button" class="action-button action-button-success" @click="applyDiscretization">覆盖数据并写入</button></div>
                          <div class="result-banner compact-banner"><span class="result-icon">✓</span><div><strong>{{ discretization.message || '请输入参数并确认分割' }}</strong><p v-if="discretization.lambda !== null">Box-Cox λ = {{ discretization.lambda }}</p></div></div>
                        </CollapsibleSidebar>
                        <div class="module-main">
                          <div class="tool-heading module-heading"><div class="tool-heading-index amber">C</div><div><span class="section-kicker">TRANSFORM</span><h3>数值变量离散化</h3><p>参数与结果按 R Shiny 的版块关系排列：左侧是数据变换，图表内是图表专属参数。</p></div><el-tag effect="plain">数据变换</el-tag></div>
                          <div class="chart-grid analysis-chart-grid discretization-grid">
                            <section class="content-panel chart-card"><div class="chart-card-heading"><strong>原数据频数分布</strong><span>{{ discretization.transform }}</span></div><div class="chart-bound-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.discretizationChart }"><CollapsibleSidebar v-model="sidebarCollapsed.discretizationChart" class="chart-local-controls" title="图表参数" eyebrow="CHART" icon="⌁"><div class="control-field"><span class="control-label">正态变换</span><el-select v-model="discretization.transform"><el-option label="原数据" value="原数据" /><el-option label="Box-Cox" value="Box-Cox" /></el-select></div><div class="control-field"><span class="control-label">直方数量</span><el-input-number v-model="discretization.bins" controls-position="right" :min="1" :max="999999" /></div><small>仅影响左侧频数图</small></CollapsibleSidebar><div class="chart-bound-content"><PlotlyChart :spec="discretization.spec" height="390px" /></div></div></section>
                            <section class="content-panel chart-card"><div class="chart-card-heading"><strong>原始分位数表</strong><span>0%–100%</span></div><PaginatedTable v-if="discretization.distributionRows.length" :rows="discretization.distributionRows" :columns="['分位数', '数值', '小于等于该值的样本数']" height="390" record-label="个分位点" /><div v-else class="inline-empty">确认分割后显示分位数表</div></section>
                            <section class="content-panel chart-card"><div class="chart-card-heading"><strong>离散化分组分布</strong><span>频数 / 占比</span></div><PlotlyChart :spec="discretization.discretizedSpec" height="430px" /></section>
                            <section class="content-panel chart-card"><div class="chart-card-heading"><strong>离散化分组统计</strong><span>区间范围</span></div><PaginatedTable v-if="discretization.groupRows.length" :rows="discretization.groupRows" :columns="Object.keys(discretization.groupRows[0])" height="390" record-label="个分组" /><div v-else class="inline-empty">确认分割后显示分组统计</div></section>
                          </div>
                        </div>
                      </div>
                    </el-tab-pane>

                    <el-tab-pane name="qualitative">
                      <template #label>探索性定性分析</template>
                      <div class="module-layout analysis-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.qualitative }">
                        <CollapsibleSidebar v-model="sidebarCollapsed.qualitative" class="module-control-sidebar" title="分析参数" eyebrow="QUALITATIVE" icon="D" tone="green">
                          <p class="module-help">一级分层变量、颜色和排序是四张图共用的全局参数。</p>
                          <div class="module-control-list">
                            <div class="control-field"><span class="control-label">一级分层变量</span><el-select v-model="qualitative.primary" filterable placeholder="选择主变量"><el-option v-for="column in qualitativeColumns" :key="column" :label="column" :value="column" /></el-select></div>
                            <div class="control-field"><span class="control-label">颜色模式</span><el-select v-model="qualitative.colorMode"><el-option label="默认颜色" value="默认颜色" /><el-option label="哈希颜色" value="哈希颜色" /></el-select></div>
                            <div class="control-field"><span class="control-label">图例排序</span><button type="button" class="action-button" @click="changeQualitativeOrder">{{ qualitative.orderDirection > 0 ? '正序显示图例' : '逆序显示图例' }}</button></div>
                          </div>
                          <div class="module-actions"><button type="button" class="action-button action-button-primary" @click="loadQualitative">计算并更新图表</button></div>
                        </CollapsibleSidebar>
                        <div class="module-main">
                          <div class="tool-heading module-heading"><div class="tool-heading-index green">D</div><div><span class="section-kicker">QUALITATIVE EXPLORATION</span><h3>探索性定性分析</h3><p>每张图旁边只放与该图绑定的控件，避免在页面顶部来回寻找参数。</p></div><el-tag type="success" effect="plain">多视图</el-tag></div>
                          <div class="chart-grid analysis-chart-grid qualitative-grid">
                            <section class="content-panel chart-card"><div class="chart-card-heading"><strong>构成概览</strong><span>一级分层</span></div><PlotlyChart :spec="qualitative.pie" height="430px" /></section>
                             <section class="content-panel chart-card"><div class="chart-card-heading"><strong>连续堆叠（上）</strong><span>{{ qualitative.topXVar }}</span></div><div class="chart-bound-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.qualitativeTop }"><CollapsibleSidebar v-model="sidebarCollapsed.qualitativeTop" class="chart-local-controls" title="上图参数" eyebrow="CHART" icon="⌁"><div class="control-field"><span class="control-label">连续型变量</span><el-select v-model="qualitative.topXVar"><el-option label="采样时间" value="采样时间" /><el-option label="年龄" value="年龄" /><el-option label="定量结果" value="定量结果" /></el-select></div><div class="control-field"><span class="control-label">纵轴显示</span><el-select v-model="qualitative.topYMode"><el-option label="数量" value="数量" /><el-option label="占比" value="占比" /></el-select></div><div class="control-field"><span class="control-label">分组粒度</span><el-select v-if="qualitative.topXVar === '采样时间'" v-model="qualitative.topGrain"><el-option label="月" value="month" /><el-option label="周" value="week" /><el-option label="日" value="day" /></el-select><el-select v-else-if="qualitative.topXVar === '年龄'" v-model="qualitative.topGrain"><el-option v-for="value in ['1','3','5','10']" :key="value" :label="value" :value="value" /></el-select><el-input-number v-else v-model="qualitative.topGrain" :min="0.001" controls-position="right" /></div><el-checkbox v-model="qualitative.topNormalize" :disabled="qualitative.topXVar !== '定量结果'">Box-Cox 正态化</el-checkbox></CollapsibleSidebar><div class="chart-bound-content"><PlotlyChart :spec="qualitative.consecutiveTop" height="430px" /></div></div></section>
                             <section class="content-panel chart-card"><div class="chart-card-heading"><strong>离散交叉堆叠</strong><span>二级分层</span></div><div class="chart-bound-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.qualitativeDiscrete }"><CollapsibleSidebar v-model="sidebarCollapsed.qualitativeDiscrete" class="chart-local-controls" title="离散图参数" eyebrow="CHART" icon="⌁"><div class="control-field"><span class="control-label">二级分层变量</span><el-select v-model="qualitative.secondary" filterable clearable placeholder="选择辅助变量"><el-option v-for="column in analysisColumns" :key="column" :label="column" :value="column" /></el-select></div><div class="control-field"><span class="control-label">堆叠方式</span><el-select v-model="qualitative.discreteYMode"><el-option label="数量" value="数量" /><el-option label="比例" value="比例" /></el-select></div><div class="control-field"><span class="control-label">上色前 Top-N</span><el-input-number v-model="qualitative.elements" controls-position="right" :min="0" :max="100" /></div></CollapsibleSidebar><div class="chart-bound-content"><PlotlyChart :spec="qualitative.discrete" height="430px" /></div></div></section>
                             <section class="content-panel chart-card"><div class="chart-card-heading"><strong>连续堆叠（下）</strong><span>{{ qualitative.bottomXVar }}</span></div><div class="chart-bound-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.qualitativeBottom }"><CollapsibleSidebar v-model="sidebarCollapsed.qualitativeBottom" class="chart-local-controls" title="下图参数" eyebrow="CHART" icon="⌁"><div class="control-field"><span class="control-label">连续型变量</span><el-select v-model="qualitative.bottomXVar"><el-option label="年龄" value="年龄" /><el-option label="采样时间" value="采样时间" /><el-option label="定量结果" value="定量结果" /></el-select></div><div class="control-field"><span class="control-label">纵轴显示</span><el-select v-model="qualitative.bottomYMode"><el-option label="数量" value="数量" /><el-option label="占比" value="占比" /></el-select></div><div class="control-field"><span class="control-label">分组粒度</span><el-select v-if="qualitative.bottomXVar === '采样时间'" v-model="qualitative.bottomGrain"><el-option label="月" value="month" /><el-option label="周" value="week" /><el-option label="日" value="day" /></el-select><el-select v-else-if="qualitative.bottomXVar === '年龄'" v-model="qualitative.bottomGrain"><el-option v-for="value in ['1','3','5','10']" :key="value" :label="value" :value="value" /></el-select><el-input-number v-else v-model="qualitative.bottomGrain" :min="0.001" controls-position="right" /></div><el-checkbox v-model="qualitative.bottomNormalize" :disabled="qualitative.bottomXVar !== '定量结果'">Box-Cox 正态化</el-checkbox></CollapsibleSidebar><div class="chart-bound-content"><PlotlyChart :spec="qualitative.consecutiveBottom" height="430px" /></div></div></section>
                          </div>
                        </div>
                      </div>
                    </el-tab-pane>

                    <el-tab-pane name="quantitative">
                      <template #label>探索性定量分析</template>
                      <div class="module-layout analysis-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.quantitative }">
                        <CollapsibleSidebar v-model="sidebarCollapsed.quantitative" class="module-control-sidebar" title="趋势分析参数" eyebrow="QUANTITATIVE" icon="E" tone="blue">
                          <p class="module-help">所有参数都只绑定右侧分层分位数趋势图。</p>
                          <div class="module-control-list">
                            <div class="control-field"><span class="control-label">颜色代表的分层变量</span><el-select v-model="quantitative.grouping" filterable placeholder="选择分层变量"><el-option v-for="column in analysisColumns" :key="column" :label="column" :value="column" /></el-select></div>
                            <div class="control-field"><span class="control-label">悬停模式</span><el-select v-model="quantitative.hoverMode"><el-option label="x unified" value="x unified" /><el-option label="constant" value="constant" /></el-select></div>
                            <div class="control-field"><span class="control-label">分位数曲线</span><el-select v-model="quantitative.ci" multiple collapse-tags filterable placeholder="选择曲线"><el-option v-for="value in ['99%','95%','90%','80%','50%','20%','10%','5%','1%']" :key="value" :label="value" :value="value" /></el-select></div>
                            <div class="control-field slider-field"><span class="control-label">平滑度 <em>{{ quantitative.smoothing.toFixed(2) }}</em></span><el-slider v-model="quantitative.smoothing" :min="0" :max="1.3" :step="0.01" /></div>
                            <div class="control-field"><span class="control-label">年龄窗口</span><el-input-number v-model="quantitative.winWidth" controls-position="right" :min="1" :max="29" :step="2" /></div>
                            <div class="control-field"><span class="control-label">最小样本量</span><el-input-number v-model="quantitative.minNum" controls-position="right" :min="1" :max="100" /></div>
                          </div>
                          <div class="module-actions"><button type="button" class="action-button action-button-primary" @click="loadQuantitative">计算趋势</button></div>
                        </CollapsibleSidebar>
                        <div class="module-main">
                          <div class="tool-heading module-heading"><div class="tool-heading-index blue">E</div><div><span class="section-kicker">QUANTITATIVE EXPLORATION</span><h3>分层分位数趋势 / 散点图</h3><p>参数调整后重新计算，结果集中在右侧图表区域。</p></div><el-tag type="primary" effect="plain">趋势分析</el-tag></div>
                          <div class="chart-stage quantitative-chart-stage"><PlotlyChart :spec="quantitative.spec" height="650px" /></div>
                        </div>
                      </div>
                    </el-tab-pane>

                    <el-tab-pane name="batch">
                      <template #label>批间差异分析</template>
                      <div class="module-layout analysis-module-layout batch-module-layout" :class="{ 'sidebar-collapsed': sidebarCollapsed.batch }">
                        <CollapsibleSidebar v-model="sidebarCollapsed.batch" class="module-control-sidebar" title="分析参数" eyebrow="BATCH DIFFERENCE" icon="F" tone="rose">
                          <p class="module-help">公式、窗口样本数和步长共同决定右侧两个图表及明细表。</p>
                          <div class="module-control-list">
                            <div class="control-field"><span class="control-label">回归公式自变量</span><el-input v-model="batch.formula" type="textarea" :rows="3" placeholder="例如：性别:I(年龄^2) + 性别:年龄 + 类别_无监督" @blur="validateBatchFormula" /></div>
                            <div class="control-field"><span class="control-label">窗口样本数</span><el-input-number v-model="batch.n" controls-position="right" :min="10" :max="1000" :step="10" /></div>
                            <div class="control-field"><span class="control-label">窗口步长</span><el-input-number v-model="batch.step" controls-position="right" :min="10" :max="1000" :step="10" /></div>
                          </div>
                          <div v-if="batch.formulaMessage" class="formula-validation" :class="{ valid: batch.formulaValid }">{{ batch.formulaValid ? '✓' : '⚠' }} {{ batch.formulaMessage }}</div>
                          <div class="module-actions"><button type="button" class="action-button action-button-primary" :class="{ 'is-loading': batch.job?.status === 'running' }" :disabled="batch.job?.status === 'running'" @click="startBatch"><span v-if="batch.job?.status === 'running'" class="button-spinner" />{{ batch.job?.status === 'running' ? '计算中…' : '开始计算' }}</button></div>
                          <div v-if="batch.job" class="job-state"><div><span class="job-status-badge" :class="`job-status-${batch.job.status}`">{{ jobStatusText(batch.job.status) }}</span><span>{{ batch.job.detail || '正在计算批间差异…' }}</span></div><div v-if="batch.job.status !== 'completed'" class="task-progress"><span :style="{ width: `${Math.round((batch.job.progress || 0) * 100)}%` }" /></div><small v-if="batch.job.status !== 'completed'">{{ Math.round((batch.job.progress || 0) * 100) }}%</small></div>
                          <div class="formula-preview compact-formula"><span>FORMULA</span><code>定量结果_transformed ~ {{ batch.formula }}</code></div>
                        </CollapsibleSidebar>
                        <div class="module-main">
                           <div class="tool-heading module-heading"><div class="tool-heading-index rose">F</div><div><span class="section-kicker">BATCH DIFFERENCE</span><h3>批间差异分析</h3><p>基于 Box-Cox、协变量回归残差和滑动窗口分位数检验。</p></div><div class="module-heading-actions"><button type="button" class="action-button action-button-small" @click="batchMethodVisible = true">方法说明</button><el-tag type="danger" effect="plain">统计任务</el-tag></div></div>
                          <el-tabs v-model="batchChartTab" class="chart-tabs batch-chart-tabs">
                            <el-tab-pane name="line" label="折线图"><section class="content-panel chart-card batch-single-chart"><div class="chart-card-heading"><strong>多厂家多分位数等效水平分布</strong><span>红 ≤0.01 · 橙 ≤0.05 · 绿 &gt;0.05</span></div><PlotlyChart :spec="batchLineSpec" @click="inspectBatchPlot" /></section></el-tab-pane>
                            <el-tab-pane name="violin" label="小提琴图"><section class="content-panel chart-card batch-single-chart"><div class="chart-card-heading"><strong>多厂家各分位数等效波动分布</strong><span>按分位数分面</span></div><PlotlyChart :spec="batchViolinSpec" @click="inspectBatchPlot" /></section></el-tab-pane>
                          </el-tabs>
                           <div class="subpanel-heading result-heading"><div><strong>明细结果</strong><span>颜色表示调整后 p 值；点击任意结果点查看原始数据、密度和协变量富集</span></div><span v-if="batch.rows.length">{{ batch.rows.length }} 个结果点</span></div>
                           <PaginatedTable v-if="batch.rows.length" :rows="batch.rows" :columns="['point_id', 'manu_name', 'quantile_level', 'win_sample_start', 'win_sample_stop', '等效水平', '等效波动', 'p_value_bonferroni']" height="560" record-label="个结果点" class="result-table-view" @row-click="inspectBatch"><template #default><el-table-column prop="point_id" label="结果点" min-width="180" show-overflow-tooltip /><el-table-column prop="manu_name" label="厂家" min-width="120" /><el-table-column prop="quantile_level" label="分位数" min-width="100" /><el-table-column prop="win_sample_start" label="窗口起点" width="90" /><el-table-column prop="win_sample_stop" label="窗口终点" width="90" /><el-table-column prop="等效水平" label="等效水平" min-width="120" /><el-table-column prop="等效波动" label="等效波动" min-width="120" /><el-table-column prop="p_value_bonferroni" label="调整后 p 值（BH）" min-width="150" /></template></PaginatedTable><div v-else class="inline-empty">完成一次批间差异分析后显示结果。</div>
                          <el-dialog v-model="batch.dialogVisible" title="结果解读" width="92%" top="4vh" destroy-on-close>
                            <div v-if="batch.detail" class="batch-detail">
                              <div class="detail-summary"><strong>{{ batch.detail.point?.manu_name }}</strong><span>{{ batch.detail.point?.quantile_level }} · 窗口 {{ batch.detail.point?.win_sample_start }}–{{ batch.detail.point?.win_sample_stop }}</span><el-tag v-if="batch.detail.is_all_green" type="success">全绿通过</el-tag><span>{{ batch.detail.batch_lot_info }}</span></div>
                              <el-tabs v-model="batchDetailTab" class="detail-tabs">
                                <el-tab-pane name="detail" label="详细解读">
                                  <div class="interpretation detail-interpretation"><span class="section-kicker">综合结论</span><div v-html="batch.detail.interpretation" /></div>
                                  <div class="detail-section"><h4>分布对比分析</h4><p>对照组为所有全绿 batch 的数据集合。</p><PlotlyChart :spec="batch.detail.density" height="420px" /></div>
                                  <div class="detail-section"><h4>协变量富集分析</h4><p>分析测试 batch 与全绿 batch 对照组在协变量分布上的差异。</p><PaginatedTable v-if="batch.detail.enrichment?.length" :rows="batch.detail.enrichment" :columns="Object.keys(batch.detail.enrichment[0])" height="420" record-label="条富集结果" /><div v-else class="inline-empty">暂无可进行的协变量富集分析</div></div>
                                </el-tab-pane>
                                <el-tab-pane name="raw" label="原始数据"><PaginatedTable v-if="batch.detail.raw_rows?.length" :rows="batch.detail.raw_rows" :columns="Object.keys(batch.detail.raw_rows[0])" height="520" record-label="条原始记录" /><div v-else class="inline-empty">暂无原始数据</div></el-tab-pane>
                              </el-tabs>
                            </div>
                           </el-dialog>
                           <el-dialog v-model="batchMethodVisible" title="批间差异分析：一眼看懂" width="92%" top="4vh" destroy-on-close>
                             <div class="batch-method-content">
                               <div class="method-story-hero"><span class="section-kicker">先记住这一句话</span><h3>它要找的不是“谁更好”，而是：<br /><strong>这批数据有没有突然变得不一样？</strong></h3><p>把样本按时间排开，像拿手电筒一段一段照过去。每照到一段，就检查低值、中间值和高值有没有一起偏移。</p></div>
                               <section class="method-story-flow"><div class="story-flow-title"><span>分析流水线</span><p>从原始数据，到可以行动的提醒</p></div><div class="story-flow-steps"><div class="story-flow-step"><span class="story-step-number">1</span><div><strong>先扣掉背景影响</strong><p>年龄、性别、类别等因素可能让结果不同。模型先把这些影响扣除，只留下更像“厂家 / 时间变化”的部分。</p><small>这就是“协变量控制”</small></div></div><i class="story-flow-arrow">→</i><div class="story-flow-step"><span class="story-step-number">2</span><div><strong>沿时间切成小窗口</strong><p>按采样时间往前滑动，每次看一段样本。窗口越小，越容易定位变化发生在哪一段。</p><small>窗口数 n · 步长 step</small></div></div><i class="story-flow-arrow">→</i><div class="story-flow-step"><span class="story-step-number">3</span><div><strong>同时看低、中、高</strong><p>不只看平均数，而是看 10%、30%、50%、70%、90% 五个位置，判断是整体移动还是只影响某个浓度区间。</p><small>五条分位数信号</small></div></div><i class="story-flow-arrow">→</i><div class="story-flow-step"><span class="story-step-number">4</span><div><strong>亮灯提醒你核查</strong><p>红色优先看，橙色值得关注，绿色暂未发现显著异常。点一下结果，还能回到原始数据核对。</p><small>统计信号，不是最终判定</small></div></div></div></section>
                               <section class="method-story-section"><div class="story-section-title"><span class="story-title-mark">看颜色</span><div><h4>颜色告诉你“要不要先看这里”</h4><p>颜色来自调整后的 p 值；它描述异常信号的强弱，不描述变化有多大。</p></div></div><div class="story-signal-grid"><div class="story-signal-card story-signal-red"><span>红灯</span><strong>优先核查</strong><p>p ≤ 0.01<br />信号很强，先检查批号、校准和质控。</p></div><div class="story-signal-card story-signal-orange"><span>黄灯</span><strong>值得关注</strong><p>0.01 &lt; p ≤ 0.05<br />已有统计信号，结合波动大小继续判断。</p></div><div class="story-signal-card story-signal-green"><span>绿灯</span><strong>暂未发现显著异常</strong><p>p &gt; 0.05<br />不是“证明完全一样”，而是当前没有足够证据报警。</p></div></div></section>
                               <section class="method-story-section"><div class="story-section-title"><span class="story-title-mark">看结果</span><div><h4>四个问题，帮你读懂一扇窗口</h4><p>建议先看同一窗口的五个分位数，再看具体数值和原始记录。</p></div></div><div class="story-reading-list"><div><b>它变高了还是变低了？</b><p>看“等效波动”：正数是升高，负数是降低。数值越大，实际变化幅度越大。</p></div><div><b>是整个范围都变了，还是只有一段变了？</b><p>低、中、高分位数一起同向，像整体平移；低端和高端方向相反，可能是分布压缩、扩展或非线性问题。</p></div><div><b>这个信号需要做什么？</b><p>红灯先核查；部分分位数异常时，重点看对应浓度区间；绿灯则继续结合质控和可接受限观察。</p></div></div></section>
                               <section class="method-story-section"><div class="story-section-title"><span class="story-title-mark">点进去看</span><div><h4>详情页是一条“证据链”</h4><p>不要停在颜色上，按下面顺序把原因找出来。</p></div></div><div class="story-evidence-list"><div><span>1</span><div><strong>先看综合结论</strong><p>系统会告诉你是整体升高、整体降低、局部变化，还是暂未发现显著偏差。</p></div></div><div><span>2</span><div><strong>再看分布对比</strong><p>测试窗口和稳定窗口放在一起，看整体位置、宽度和形状有没有改变。</p></div></div><div><span>3</span><div><strong>最后回到原始数据</strong><p>核对批号、盒号、采样时间、异常值和协变量，决定是否需要进一步调查。</p></div></div></div></section>
                               <details class="method-technical-details"><summary>想了解统计细节，再展开这里</summary><div><p><strong>模型：</strong>先对定量结果做 Box-Cox 转换，再按厂家拟合 <code>定量结果_transformed ~ 当前公式</code>，用回归残差表示扣除协变量后的变化。</p><p><strong>窗口检验：</strong>每个窗口、每个分位数用顺序统计量和 Beta CDF 构造双侧 p 值，不是普通的均值 t 检验。</p><p><strong>多重比较：</strong>同一厂家、同一分位数的窗口 p 值使用 Benjamini–Hochberg FDR 调整。结果字段虽然历史命名为 <code>p_value_bonferroni</code>，实际采用的是 BH。</p><p><strong>详情检验：</strong>数值协变量根据正态性选择 Welch t 检验或 Mann–Whitney U 检验；分类协变量使用卡方或 Fisher 精确检验，并再次做 BH 调整。</p></div></details>
                               <div class="method-story-warning"><strong>最后请记住</strong><span>统计信号不是质量结论。最终要把颜色、变化幅度、受影响的浓度区间、批号 / 盒号、质控、校准记录和预先定义的可接受限放在一起判断。</span></div>
                             </div>
                           </el-dialog>
                         </div>
                      </div>
                    </el-tab-pane>
                  </el-tabs>
                </div>
              </div>
            </el-tab-pane>
          </el-tabs>
        </div>
      </main>
    </div>
  </div>
</template>
