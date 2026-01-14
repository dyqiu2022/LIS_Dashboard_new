#' qualitative_analysis_consecutive_hist UI Function
#'
#' @description
#' 连续型变量（如年龄、采样时间、定量结果）按区间离散后，
#' 以 `header_for_stack` 作为填充分组，绘制可堆叠直方图的模块。
#' 该模块设计为**可复用**，可在同一页中调用两次，
#' 例如：上图看"数量"，下图看"占比"；或者一个按年龄、一个按采样时间。
#'
#' @param id 模块 id
#' @param x_choices 可选的连续变量名称字符向量，默认包含
#'   `c("年龄", "采样时间", "定量结果")`，可以在 UI 中按需裁剪。
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput checkboxInput numericInput updateNumericInput selectInput
#' @importFrom bslib card card_body sidebar layout_sidebar
#' @importFrom plotly plotlyOutput
mod_qualitative_analysis_consecutive_hist_ui <- function(id,
                                                         x_choices = c("年龄", "采样时间", "定量结果")) {
  ns <- NS(id)

  layout_sidebar(
    fillable = TRUE,
    padding  = 0,
    gap      = "20px",
    class    = "h-100",
    sidebar = sidebar(
      bg   = "#f8f9fa",
      open = "desktop",
      selectInput(
        ns("x_var"),
        label   = "连续型变量",
        choices = x_choices,
        selected = x_choices[[1]]
      ),
      selectInput(
        ns("y_mode"),
        label   = "纵轴显示",
        choices = c("数量", "占比"),
        selected = "数量"
      ),
      # 动态控件：根据选择的变量显示不同的颗粒度选项
      uiOutput(ns("grain_controls"))
    ),
    card(
      full_screen = TRUE,
      style = "aspect-ratio: 3/2; overflow: hidden;",
      card_body(
        padding = 0,
        plotly::plotlyOutput(ns("consecutive_hist_plot"),
                             height = "100%", width = "100%")
      )
    )
  )
}

#' 内部工具函数：计算连续变量堆叠直方图所需数据
#'
#' @noRd
#'
#' @importFrom dplyr group_by summarise ungroup mutate arrange n
#' @importFrom rlang sym .data
calc_consecutive_hist_data <- function(data,
                                       x_var,
                                       group_var,
                                       grain_value,
                                       normalize_quantitative = FALSE,
                                       order_para = 1) {
  # 基础检查
  stopifnot(is.character(x_var), length(x_var) == 1)
  stopifnot(is.character(group_var), length(group_var) == 1)

  current_data <- data
  bin_col  <- NULL
  x_label  <- NULL
  ratio_col <- NULL

  if (x_var == "年龄") {
    if (!is.numeric(current_data[["年龄"]])) {
      stop("年龄含有非数值水平")
    }
    current_data$年龄组 <- discretize_age(
      current_data$年龄,
      grain = as.numeric(grain_value)
    )
    bin_col  <- "年龄组"
    x_label  <- "年龄"
    ratio_col <- "当前年龄占比"
  } else if (x_var == "采样时间") {
    if (!inherits(current_data[["采样时间"]], c("POSIXt", "Date"))) {
      stop("采样时间含有非日期水平")
    }
    current_data$时间组 <- discretize_time(
      current_data$采样时间,
      grain = grain_value
    )
    bin_col  <- "时间组"
    x_label  <- "时间"
    ratio_col <- "当前时间组占比"
  } else if (x_var == "定量结果") {
    if (!is.numeric(current_data[["定量结果"]])) {
      stop("定量结果含有非数值水平")
    }
    
    # 处理定量结果：先进行Box-Cox转换（如果需要）
    quant_data <- current_data[["定量结果"]]
    
    if (isTRUE(normalize_quantitative)) {
      quant_data <- maxlikelihood_boxcox(quant_data)
    }
    
    # 使用自定义颗粒度进行离散化
    # grain_value 此时应该是数值（自定义的颗粒度）
    current_data$定量结果组 <- discretize_age(
      quant_data,
      grain = as.numeric(grain_value)
    )
    bin_col  <- "定量结果组"
    x_label  <- if (normalize_quantitative) "定量结果（Box-Cox转换后）" else "定量结果"
    ratio_col <- "当前定量结果占比"
  } else {
    stop("不支持的连续变量：", x_var)
  }

  # 聚合并补全空分组
  selected_header <- group_var

  plot_data <- current_data %>%
    dplyr::group_by(.data[[bin_col]], .data[[selected_header]]) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # 每个 x 组的总数与占比
  sum_data <- plot_data %>%
    dplyr::group_by(.data[[bin_col]]) %>%
    dplyr::summarise(sum_n = sum(.data$n), .groups = "drop")

  plot_data <- merge(plot_data, sum_data, by = bin_col)
  plot_data[[ratio_col]] <- plot_data$n / plot_data$sum_n

  # 依据整体数量决定填充变量（group_var）的顺序
  order_data <- plot_data %>%
    dplyr::group_by(.data[[selected_header]]) %>%
    dplyr::summarise(sum_ = sum(.data$n), .groups = "drop") %>%
    dplyr::arrange(order_para * .data$sum_)

  plot_data[[selected_header]] <- factor(
    plot_data[[selected_header]],
    levels = order_data[[selected_header]] %>% unlist()
  )

  # 颜色顺序（用于哈希颜色）
  color_vec <- lapply(
    unlist(order_data[[selected_header]]),
    string_to_color
  )

  # 固定 x 轴分组顺序
  plot_data[[bin_col]] <- factor(
    plot_data[[bin_col]],
    levels = unique(plot_data[[bin_col]])
  )

  list(
    plot_data = plot_data,
    color_vec = color_vec,
    bin_col   = bin_col,
    x_label   = x_label,
    ratio_col = ratio_col
  )
}

#' qualitative_analysis_consecutive_hist Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer req validate need reactive observeEvent renderUI tagList
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_col scale_fill_manual labs theme_minimal theme element_text
mod_qualitative_analysis_consecutive_hist_server <- function(id,
                                                              global_store,
                                                              header_for_stack,
                                                              hash_color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 计算定量结果的默认颗粒度（range/200）
    default_quantitative_grain <- reactive({
      req(global_store[["filtered_data"]])
      data <- global_store[["filtered_data"]]
      if (!is.numeric(data[["定量结果"]])) {
        return(1)
      }
      quant_range <- range(data[["定量结果"]], na.rm = TRUE)
      default_grain <- (quant_range[2] - quant_range[1]) / 200
      # 确保颗粒度至少为1
      max(1, default_grain)
    })
    
    # 当选择定量结果且需要正态化时，计算转换后的默认颗粒度
    default_normalized_quantitative_grain <- reactive({
      req(global_store[["filtered_data"]])
      req(input$x_var == "定量结果")
      
      data <- global_store[["filtered_data"]]
      if (!is.numeric(data[["定量结果"]])) {
        return(1)
      }
      
      # 进行Box-Cox转换
      quant_data <- maxlikelihood_boxcox(data[["定量结果"]])
      quant_range <- range(quant_data, na.rm = TRUE)
      default_grain <- (quant_range[2] - quant_range[1]) / 200
      max(1, default_grain)
    })
    
    # 动态生成颗粒度控件
    output$grain_controls <- renderUI({
      req(input$x_var)
      
      if (input$x_var == "年龄") {
        # 年龄：固定选项 1, 3, 5, 10
        selectInput(
          ns("grain_value"),
          label = "年龄颗粒度",
          choices = c("1", "3", "5", "10"),
          selected = "3",
          width = "100%"
        )
      } else if (input$x_var == "采样时间") {
        # 时间：month, week, day
        selectInput(
          ns("grain_value"),
          label = "时间颗粒度",
          choices = c("month", "week", "day"),
          selected = "month",
          width = "100%"
        )
      } else if (input$x_var == "定量结果") {
        # 定量结果：自定义颗粒度 + 是否正态化
        normalize_val <- if (!is.null(input$normalize_quantitative)) {
          input$normalize_quantitative
        } else {
          FALSE
        }
        
        grain_default_val <- tryCatch({
          if (isTRUE(normalize_val)) {
            default_normalized_quantitative_grain()
          } else {
            default_quantitative_grain()
          }
        }, error = function(e) {
          1  # 如果计算失败，返回默认值1
        })
        
        tagList(
          checkboxInput(
            ns("normalize_quantitative"),
            label = "Box-Cox正态化",
            value = FALSE,
            width = "100%"
          ),
          numericInput(
            ns("grain_value"),
            label = "定量结果颗粒度",
            value = grain_default_val,
            min = 0.001,
            step = 0.1,
            width = "100%"
          )
        )
      }
    })
    
    # 当变量类型改变时，重置正态化选项（如果切换到非定量结果）
    observeEvent(input$x_var, {
      if (input$x_var != "定量结果" && !is.null(input$normalize_quantitative)) {
        # 切换到非定量结果变量时，不需要重置，因为控件会动态生成
      }
    })
    
    # 当正态化选项改变时，更新颗粒度默认值
    observeEvent(input$normalize_quantitative, {
      if (input$x_var == "定量结果" && !is.null(input$normalize_quantitative)) {
        new_value <- tryCatch({
          if (isTRUE(input$normalize_quantitative)) {
            default_normalized_quantitative_grain()
          } else {
            default_quantitative_grain()
          }
        }, error = function(e) {
          # 如果计算失败，保持当前值或使用默认值1
          if (!is.null(input$grain_value)) {
            input$grain_value
          } else {
            1
          }
        })
        
        if (!is.null(new_value) && is.numeric(new_value)) {
          updateNumericInput(
            session,
            "grain_value",
            value = new_value
          )
        }
      }
    })

    output$consecutive_hist_plot <- plotly::renderPlotly({
      shiny::req(global_store[["filtered_data"]])
      shiny::req(header_for_stack())
      shiny::req(hash_color())
      shiny::req(input$x_var)
      shiny::req(input$grain_value)

      data <- global_store[["filtered_data"]]
      shiny::validate(shiny::need(nrow(data) > 0, message = "请上传数据"))

      x_var <- input$x_var
      group_var <- header_for_stack()
      
      # 获取正态化选项（仅对定量结果有效）
      normalize_quantitative <- if (x_var == "定量结果") {
        isTRUE(input$normalize_quantitative)
      } else {
        FALSE
      }

      # 计算排序方向
      order_para <- global_store[["order_para"]]
      if (is.null(order_para)) order_para <- 1

      # 业务逻辑：计算分组数据
      processed <- calc_consecutive_hist_data(
        data                    = data,
        x_var                   = x_var,
        group_var               = group_var,
        grain_value             = input$grain_value,
        normalize_quantitative  = normalize_quantitative,
        order_para              = order_para
      )

      plot_data <- processed$plot_data
      bin_col   <- processed$bin_col
      x_label   <- processed$x_label
      ratio_col <- processed$ratio_col

      # 颜色映射（与饼图/离散直方图逻辑保持一致）
      color_current <- processed$color_vec
      color_length  <- length(color_current)
      color_times   <- ceiling(color_length / length(color_default))

      if (hash_color() == "默认颜色") {
        color_ <- rep(color_default, color_times)
      } else {
        color_ <- color_current
      }

      # 选择 y 轴
      y_var <- if (identical(input$y_mode, "数量")) "n" else ratio_col

      title_prefix <- if (identical(input$y_mode, "数量")) "数量堆叠图" else "数量占比堆叠图"

      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes_string(
          x    = bin_col,
          y    = y_var,
          fill = group_var
        )
      ) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::scale_fill_manual(values = color_) +
        ggplot2::labs(
          title = paste0("患者", title_prefix, "（", x_label, "）"),
          x     = x_label,
          y     = if (identical(input$y_mode, "数量")) "统计数量" else "占比",
          fill  = ""
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = -60, hjust = 0)
        )

      plotly::ggplotly(p)
    }) %>% shiny::debounce(800)
  })
}

## To be copied in the UI
# mod_qualitative_analysis_consecutive_hist_ui("qualitative_analysis_consecutive_hist_1")

## To be copied in the server
# mod_qualitative_analysis_consecutive_hist_server("qualitative_analysis_consecutive_hist_1")
