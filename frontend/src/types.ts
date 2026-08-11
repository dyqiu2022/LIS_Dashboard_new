export interface DatasetInfo {
  dataset_id: string
  name: string
  row_count: number
  columns: string[]
  schema: Array<Record<string, any>>
  invalid_counts: Record<string, number>
}

export interface PlotSpec { data: any[]; layout?: Record<string, any>; [key: string]: any }
