#' show_filtered_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon downloadButton
#' @importFrom bslib card card_header card_body
#' @importFrom DT dataTableOutput
mod_show_filtered_data_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "当前数据",
    # 1. 确保 Card 占满 nav_panel 的高度
    card(
      full_screen = TRUE,
      height = "100%",

      # 2. 头部：左侧标题，右侧放自定义下载按钮
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span(icon("table"), " 数据明细表"),
        # 自定义下载按钮 (替代 DT 原生导出)
        downloadButton(ns("download_all_data"), "下载完整数据(CSV)", class = "btn-sm btn-primary")
      ),

      # 3. 主体：去掉内边距，为了让表格撑满边缘
      card_body(
        padding = 0,
        # 【核心关键点】 fill = TRUE
        # 这会让表格容器自动计算高度，填满 card_body
        DT::dataTableOutput(ns("raw_data_table"), height = "100%", fill = TRUE)
      )
    )
  )
}
#' show_filtered_data Server Functions
#'
#' @noRd
#' @importFrom DT renderDataTable datatable formatStyle styleEqual
#' @importFrom dplyr select
#' @importFrom utils write.csv
mod_show_filtered_data_server <- function(id, global_store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- 1. 下载处理器 (保持不变) ---
    output$download_all_data <- downloadHandler(
      filename = function() {
        paste0("完整筛选数据_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(global_store[["filtered_data"]])
        write.csv(global_store[["filtered_data"]], file, row.names = FALSE, fileEncoding = "GBK")
      }
    )

    # --- 2. 表格渲染 ---
    output$raw_data_table <- DT::renderDataTable({
      req(global_store[["filtered_data"]])
      data <- global_store[["filtered_data"]]
      validate(need(nrow(data) > 0, "当前筛选条件下暂无数据"))

      # 计算隐藏列索引逻辑
      target_hide_cols <- c("样本类型", "方法学", "项目序号", "结果单位", "参考区间", "备注信息", "调研人")
      actual_hide_cols <- intersect(names(data), target_hide_cols)
      hidden_indices <- which(names(data) %in% actual_hide_cols) - 1

      DT::datatable(
        data,
        # 1. 移除 'Scroller'，只保留 'Buttons'
        extensions = c('Buttons'),

        # 2. 保持 fillContainer = TRUE
        # 在翻页模式下，这会让分页条固定在 Card 底部，表格主体自动撑满中间
        fillContainer = TRUE,

        style = "bootstrap",
        class = "table table-striped table-hover table-sm display nowrap",
        rownames = FALSE,
        selection = 'none',
        filter = 'top',

        options = list(
          # 3. 开启翻页
          paging = TRUE,
          scroller = FALSE, # 显式关闭

          # 4. 设置每页默认行数 (因为是全屏，建议稍微多一点，比如 20)
          pageLength = 20,
          # 也可以开启长度菜单
          lengthMenu = c(15, 20, 50, 100),
          dom = '<"d-flex justify-content-between mb-2"Bf>t<"d-flex justify-content-between align-items-center mt-2"ilp>',

          # 按钮配置 (保持不变)
          buttons = list(
            list(
              extend = 'colvis',
              text = '显示/隐藏列',
              columns = ':not(:first-child)',
              className = 'btn-sm btn-outline-secondary'
            )
          ),

          columnDefs = list(
            list(targets = hidden_indices, visible = FALSE)
          ),

          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json',
            buttons = list(colvis = '列可见性')
          )
        )
      ) %>%
        DT::formatStyle(
          columns = names(data),
          fontSize = '14px',
          lineHeight = '20px'
        )
    })
  })
}
