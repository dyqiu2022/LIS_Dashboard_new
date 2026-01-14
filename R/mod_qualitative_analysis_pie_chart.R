#' qualitative_analysis_pie_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom bslib card card_body
#' @importFrom plotly plotlyOutput
mod_qualitative_analysis_pie_chart_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = TRUE,
    style = "aspect-ratio: 3/2; overflow: hidden;", # overflow: hidden 防止滚动条闪烁
    card_body(
      padding = 0, # 去掉内边距让图表更大
      plotly::plotlyOutput(ns("pie_plot"), height = "100%", width = "100%")
    )
  )
}

#' qualitative_analysis_pie_chart Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer req validate need reactive
#' @importFrom dplyr count rename %>% arrange desc
#' @importFrom rlang sym
#' @importFrom scales percent
#' @importFrom plotly plot_ly layout renderPlotly
mod_qualitative_analysis_pie_chart_server <- function(id, global_store, header_for_stack, hash_color){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$pie_plot <- renderPlotly({
      req(global_store[["filtered_data"]])
      req(global_store[["order_para"]])
      req(header_for_stack())
      req(hash_color())
      validate(need(nrow(global_store[["filtered_data"]])>0, message = "请上传数据"))
      pie_data <- global_store[["filtered_data"]] %>% as.data.frame
      selected_header <- header_for_stack()

      pie_data <- pie_data %>%
        count(!!sym(selected_header)) %>%
        rename(char = 1, freq = n) %>%
        as.data.frame()

      pie_data <- pie_data[order(global_store[["order_para"]]*pie_data$freq),]

      color_length <- pie_data$char %>% length()
      color_times <- (color_length/length(color_default)) %>% ceiling

      if (hash_color() == "默认颜色"){
        color_ <- rep(color_default, color_times)
      }else{
        color_ <- lapply(pie_data$char, string_to_color)
      }

      pie_data$char <- paste0(pie_data$char, " 例数:", pie_data$freq, " 占比:", ((pie_data$freq/sum(pie_data$freq)) %>% scales::percent(accuracy = 0.1)))
      pie_data$char <- factor(pie_data$char, levels = pie_data$char)



      # 直接使用 plot_ly 创建饼图
      p <- plot_ly(pie_data,
                   labels = ~char,
                   values = ~freq,
                   type = "pie",
                   textinfo = "none", # 关闭饼图本体文字
                   hoverinfo = "label", # 悬停时显示信息
                   marker = list(colors = color_),
                   sort = FALSE) %>%
        layout(
          margin = list(
            t = 30  # 增加顶部边距给主标题更多空间
          ),
          title = paste0("各", selected_header,"数量/占比"),
          showlegend = TRUE
        )

      # plots_waiting_to_download[["饼图"]] <- p

      p
    })
  })
}

## To be copied in the UI
# mod_qualitative_analysis_pie_chart_ui("qualitative_analysis_pie_chart_1")

## To be copied in the server
# mod_qualitative_analysis_pie_chart_server("qualitative_analysis_pie_chart_1")
