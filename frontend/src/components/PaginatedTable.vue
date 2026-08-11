<script setup lang="ts">
import { computed, ref, watch } from 'vue'

const props = withDefaults(defineProps<{
  rows: Record<string, any>[]
  columns?: string[]
  height?: string | number
  defaultPageSize?: number
  pageSizes?: number[]
  recordLabel?: string
  status?: string
  rowKey?: string
  loading?: boolean
}>(), {
  height: 430,
  defaultPageSize: 100,
  pageSizes: () => [20, 50, 100, 200],
  recordLabel: '条记录',
  status: '',
  loading: false,
})

const emit = defineEmits<{
  'row-click': [row: Record<string, any>]
}>()

const page = ref(1)
const pageSize = ref(props.defaultPageSize)
const total = computed(() => props.rows.length)
const tableColumns = computed(() => props.columns?.length ? props.columns : Object.keys(props.rows[0] || {}))
const pageRows = computed(() => {
  const start = (page.value - 1) * pageSize.value
  return props.rows.slice(start, start + pageSize.value)
})

watch(() => props.rows, () => {
  page.value = 1
}, { flush: 'sync' })

watch([() => props.rows.length, pageSize], () => {
  const lastPage = Math.max(1, Math.ceil(total.value / pageSize.value))
  if (page.value > lastPage) page.value = lastPage
})

function changePageSize(value: number) {
  pageSize.value = value
  page.value = 1
}

function onRowClick(row: Record<string, any>) {
  emit('row-click', row)
}
</script>

<template>
  <div class="paginated-table">
    <div class="table-caption">
      <div>
        <strong>{{ total.toLocaleString() }} {{ recordLabel }}</strong>
        <span v-if="status" class="caption-status"><i />{{ status }}</span>
      </div>
      <span>每页 {{ pageSize }} 条 · 共 {{ tableColumns.length }} 个字段</span>
    </div>
    <el-table
      :data="pageRows"
      stripe
      :height="height"
      :row-key="rowKey"
      v-loading="loading"
      @row-click="onRowClick"
    >
      <slot>
        <el-table-column v-for="column in tableColumns" :key="column" :prop="column" :label="column" min-width="130" show-overflow-tooltip />
      </slot>
    </el-table>
    <div class="table-footer">
      <span>第 {{ page }} 页</span>
      <el-pagination
        v-model:current-page="page"
        v-model:page-size="pageSize"
        background
        layout="total, sizes, prev, pager, next"
        :page-sizes="pageSizes"
        :total="total"
        @size-change="changePageSize"
      />
    </div>
  </div>
</template>
