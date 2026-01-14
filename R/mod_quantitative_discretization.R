#' quantitative_discretization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quantitative_discretization_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "定量结果离散化",
    layout_column_wrap(
      width = 1/2,        # 保持每行2列
      height = NULL,      # 关键点1：必须让高度自适应，不要锁死像素高度
      fill = FALSE,       # 告诉网格：由内容（aspect-ratio）决定高度，不要去适应父容器
      gap = "20px",       # 卡片之间的间距

      layout_sidebar(
        fillable = TRUE,
        padding = 5,
        gap = "20px",
        class = "h-100",
        # 关键点2：在每个 card 内部定义纵横比
        # 2/1 表示 宽度2 : 高度1 (即高度是宽度的0.5倍)
        card(
          style = "aspect-ratio: 5/ 3; overflow: auto;",
          full_screen = TRUE,
          card_header("图表 1"),
          div(class = "p-2", "内容区域（高度会自动变为宽度的50%）")
        )
      ),

      card(
        style = "aspect-ratio: 5/ 3; overflow: auto;",
        full_screen = TRUE,
        card_header("图表 2"),
        "内容区域 2"
      )
    ),
    layout_column_wrap(
      width = 1/2,        # 保持每行2列
      height = NULL,      # 关键点1：必须让高度自适应，不要锁死像素高度
      fill = FALSE,
      gap = "20px",       # 卡片之间的间距
      card(
        style = "aspect-ratio: 5/ 3; overflow: auto;",
        full_screen = TRUE,
        card_header("图表 3"),
        "内容区域 3"
      ),

      card(
        style = "aspect-ratio: 5/ 3; overflow: auto;",
        full_screen = TRUE,
        card_header("图表 4"),
        "内容区域 4"
      )
    )
  )
}

#' quantitative_discretization Server Functions
#'
#' @noRd
mod_quantitative_discretization_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_quantitative_discretization_ui("quantitative_discretization_1")

## To be copied in the server
# mod_quantitative_discretization_server("quantitative_discretization_1")
