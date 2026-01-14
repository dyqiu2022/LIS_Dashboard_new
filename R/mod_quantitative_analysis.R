#' quantitative_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quantitative_analysis_ui <- function(id) {
  ns <- NS(id)
  # 探索性定量分析UI
  nav_panel(
    title = "探索性定量分析",
    layout_column_wrap(
      width = 1/1,
      fill = TRUE,
      gap = "20px",       # 卡片之间的间距

      layout_sidebar(
        fillable = TRUE,
        padding = 5,
        gap = "20px",
        class = "h-100",
        sidebar = sidebar(
          title = "分析参数配置3",
          bg = "#f8f9fa",
          open = "desktop",
          "这里放置输入控件..."
        ),
        card(
          style = "height: 80vh",
          full_screen = TRUE,
          "内容区域 3"
        )
      )
    )
  )
}

#' quantitative_analysis Server Functions
#'
#' @noRd
mod_quantitative_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_quantitative_analysis_ui("quantitative_analysis_1")

## To be copied in the server
# mod_quantitative_analysis_server("quantitative_analysis_1")
