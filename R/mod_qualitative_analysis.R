#' qualitative_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qualitative_analysis_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "探索性定性分析",
    fillable = TRUE,
    # 代替原来的 fluidRow
    layout_columns(
      fill = FALSE,
      col_widths = c(2, 2, 2, 2, 2, 2), # 每个控件占 2/12 宽度
      # 或者简写为 col_widths = 2 (如果全部等宽)
      uiOutput(ns("dynamic_controls")),
      selectInput(
        ns("hash_color"),
        "颜色模式",
        choices = c("默认颜色", "哈希颜色"),
        selected = "默认颜色", width = "100%"
      ),

      # 按钮部分：bslib 不需要再手动写 margin-top 来对齐
      # 它可以用 align_vals 参数或简单的 wrapper
      div(
        class = "d-flex align-items-end h-100", # 使用 Bootstrap 5 的 Flex 布局对齐
        actionButton(
          ns("change_order"),
          "改变图例顺序",
          icon = icon("refresh"),
          class = "btn-default w-100"
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,        # 保持每行2列
      height = NULL,      # 关键点1：必须让高度自适应，不要锁死像素高度
      fill = FALSE,       # 告诉网格：由内容（aspect-ratio）决定高度，不要去适应父容器
      gap = "20px",       # 卡片之间的间距

      # 关键点2：在每个 card 内部定义纵横比
      # 2/1 表示 宽度2 : 高度1 (即高度是宽度的0.5倍)
      mod_qualitative_analysis_pie_chart_ui(ns("pie_chart")),

      # 右侧第一个连续变量堆叠图（上图）
      mod_qualitative_analysis_consecutive_hist_ui(ns("consecutive_hist_top"))
    ),


    layout_column_wrap(
      width = 1/2,        # 保持每行2列
      height = NULL,      # 关键点1：必须让高度自适应，不要锁死像素高度
      fill = FALSE,
      gap = "20px",       # 卡片之间的间距
      mod_qualitative_analysis_discrete_hist_ui(ns("discrete_hist")),

      # 右侧第二个连续变量堆叠图（下图）
      mod_qualitative_analysis_consecutive_hist_ui(ns("consecutive_hist_bottom"))
    )
  )
}

#' qualitative_analysis Server Functions
#'
#' @noRd
mod_qualitative_analysis_server <- function(id, global_store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # 动态生成控制组件
    global_store[["order_para"]] <- 1
    # 更换图例顺序
    observeEvent(input$change_order,{
      global_store[["order_para"]] <- global_store[["order_para"]]*(-1)
    })

    output$dynamic_controls <- renderUI({
      req(global_store[["filtered_data"]])
      validate(need(nrow(global_store[["filtered_data"]])>0, message = ""))

      selectInput(
        ns("header_for_stack"),
        "一级分层变量",
        choices = colnames(global_store[["filtered_data"]])[-which(colnames(global_store[["filtered_data"]]) %in% c("采样时间", "项目序号", "病人ID", "检验单号", "年龄", "临床诊断", "参考区间", "定量结果", "原始结果", "调研人"))],
        selected = "定性结果"
      )
    })

    mod_qualitative_analysis_pie_chart_server("pie_chart", global_store, reactive(input$header_for_stack), reactive(input$hash_color))
    mod_qualitative_analysis_discrete_hist_server("discrete_hist", global_store, reactive(input$header_for_stack), reactive(input$hash_color))
    
    # 右侧两个连续变量堆叠图模块（一上一下）
    mod_qualitative_analysis_consecutive_hist_server(
      "consecutive_hist_top",
      global_store,
      reactive(input$header_for_stack),
      reactive(input$hash_color)
    )
    
    mod_qualitative_analysis_consecutive_hist_server(
      "consecutive_hist_bottom",
      global_store,
      reactive(input$header_for_stack),
      reactive(input$hash_color)
    )
  })
}

## To be copied in the UI
# mod_qualitative_analysis_ui("qualitative_analysis_1")

## To be copied in the server
# mod_qualitative_analysis_server("qualitative_analysis_1")
