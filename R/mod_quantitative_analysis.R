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
  nav_panel(
    title = "探索性定量分析",
    fillable = TRUE,
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "分析参数",
        width = "170px",
        bg = "#f8f9fa",
        open = "desktop",
        uiOutput(ns("dynamic_header")),
        selectInput(
          ns("hover_mode"),
          "悬停模式",
          choices = c("x unified", "constant"),
          selected = "x unified",
          width = "100%"
        ),
        selectInput(
          ns("CI_for_plot"),
          "分位数曲线",
          choices = c("99%", "95%", "90%", "80%", "50%", "20%", "10%", "5%", "1%"),
          selected = c("95%", "80%", "50%", "20%", "5%"),
          multiple = TRUE,
          width = "100%"
        ),
        sliderInput(
          ns("smoothing_value"),
          "平滑度",
          min = 0,
          max = 1.3,
          value = 1.3,
          step = 0.01,
          width = "100%"
        ),
        sliderInput(
          ns("win_width"),
          "年龄窗口",
          min = 1,
          max = 29,
          value = 5,
          step = 2,
          width = "100%"
        ),
        sliderInput(
          ns("min_num"),
          "最小样本量",
          min = 1,
          max = 100,
          value = 20,
          step = 1,
          width = "100%"
        )
      ),
      card(
        full_screen = TRUE,
        card_header("分层分位数趋势/散点图"),
        # 参考 Rmd：宽高自适应，避免强制固定像素高度
        style = "overflow: hidden;",
        plotly::plotlyOutput(ns("quantiles_plot"), height = "100%", width = "100%")
      )
    )
  )
}

#' quantitative_analysis Server Functions
#'
#' @noRd
mod_quantitative_analysis_server <- function(id, global_store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 动态生成控制组件：颜色代表的分层变量
    output$dynamic_header <- renderUI({
      req(global_store[["reactive_data"]])
      validate(need(nrow(global_store[["reactive_data"]]) > 0, message = ""))
      selectInput(
        ns("dynamic_header_"),
        "颜色代表的分层变量",
        choices = colnames(global_store[["reactive_data"]]),
        selected = if ("性别" %in% colnames(global_store[["reactive_data"]])) "性别" else colnames(global_store[["reactive_data"]])[[1]]
      )
    })

    quant_data <- reactive({
      req(global_store[["filtered_data"]])
      req(input$CI_for_plot)
      req(input$dynamic_header_)

      validate(need(nrow(global_store[["filtered_data"]]) > 0, message = "请上传数据"))
      validate(need(is.numeric(global_store[["filtered_data"]][["年龄"]]), message = "年龄含有非数值水平"))
      validate(need(is.numeric(global_store[["filtered_data"]][["定量结果"]]), message = "定量结果含有非数值水平"))

      data_ <- global_store[["filtered_data"]]
      data_ <- data_[which(!is.na(data_$定量结果)), ]

      # 处理离群值（按 Rmd：400 倍 IQR）
      upper_quatile <- stats::quantile(data_$定量结果, 0.75, na.rm = TRUE)
      IQR_ <- stats::IQR(data_$定量结果, na.rm = TRUE)
      outlier_bar <- IQR_ * 400 + upper_quatile
      data_$定量结果[which(unlist(data_$定量结果) > outlier_bar)] <- outlier_bar

      # Box-Cox 最优 λ（按 Rmd 手动优化）
      y_values <- as.numeric(data_$定量结果) + 0.1
      lambda_seq <- seq(-2, 2, by = 0.05)
      log_likelihoods <- sapply(lambda_seq, function(lambda) {
        y_trans <- if (lambda == 0) log(y_values) else (y_values^lambda - 1) / lambda
        n <- length(y_trans)
        var_y <- stats::var(y_trans)
        if (is.na(var_y) || var_y == 0) return(-Inf)
        -n / 2 * log(var_y) + (lambda - 1) * sum(log(y_values))
      })
      best_lambda <- lambda_seq[which.max(log_likelihoods)]

      data_$boxcox_result <- if (best_lambda == 0) {
        log(data_$定量结果 + 0.1)
      } else {
        ((data_$定量结果 + 0.1)^best_lambda - 1) / best_lambda
      }

      CI <- input$CI_for_plot
      condition <- input$dynamic_header_
      extreme_quantiles <- c(CI[[1]], CI[[length(CI)]])

      count <- 0
      CI_data <- list()
      for (i in unique(data_[[condition]])) {
        count <- count + 1
        result_ <- get_CI(
          data_ %>% dplyr::filter(.data[[condition]] == i),
          CI = CI,
          win_width = input$win_width,
          min_num = input$min_num
        )
        if (nrow(result_) == 0) next
        CI_data[[count]] <- result_
        CI_data[[count]]$condition <- i
      }
      CI_data <- do.call(rbind, CI_data)

      list(data_, CI_data, extreme_quantiles, best_lambda)
    })

    output$quantiles_plot <- plotly::renderPlotly({
      req(quant_data())
      p <- plot_quantile_lines(
        data = quant_data()[[2]],
        origin_data = quant_data()[[1]],
        best_lambda = quant_data()[[4]],
        header_name = input$dynamic_header_,
        extreme_quantiles = quant_data()[[3]],
        hover_mode = input$hover_mode,
        smoothing_value = input$smoothing_value,
        title_var = input$dynamic_header_
      )
      p
    })
  })
}

## To be copied in the UI
# mod_quantitative_analysis_ui("quantitative_analysis_1")

## To be copied in the server
# mod_quantitative_analysis_server("quantitative_analysis_1")
