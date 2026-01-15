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
#' @param default_x_var 默认选中的连续型变量；若不在 `x_choices` 中则回退到 `x_choices[[1]]`。
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput checkboxInput numericInput updateNumericInput selectInput
#' @importFrom bslib card card_body sidebar layout_sidebar
#' @importFrom plotly plotlyOutput
mod_qualitative_analysis_consecutive_hist_ui <- function(id,
                                                         x_choices = c("年龄", "采样时间", "定量结果"),
                                                         default_x_var = x_choices[[1]]) {
  ns <- NS(id)
  if (!default_x_var %in% x_choices) default_x_var <- x_choices[[1]]

  layout_sidebar(
    fillable = TRUE,
    padding  = 0,
    gap      = "20px",
    class    = "h-100",
    sidebar = sidebar(
      bg   = "#f8f9fa",
      open = "desktop",
      width = "150px",
      selectInput(
        ns("x_var"),
        label   = "连续型变量",
        choices = x_choices,
        selected = default_x_var
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
    grain_value <- suppressWarnings(as.numeric(grain_value))
    if (!is.numeric(current_data[["年龄"]])) {
      stop("年龄含有非数值水平")
    }
    current_data$年龄组 <- discretize_age(
      current_data$年龄,
      grain = grain_value
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
    grain_value <- suppressWarnings(as.numeric(grain_value))
    if (!is.numeric(current_data[["定量结果"]])) {
      stop("定量结果含有非数值水平")
    }
    
    # 处理定量结果：先进行Box-Cox转换（如果需要）
    quant_data <- current_data[["定量结果"]]
    boxcox_lambda <- NULL
    boxcox_offset <- 0  # 记录是否有平移
    
    if (isTRUE(normalize_quantitative)) {
      # 进行Box-Cox转换，获取转换后的数据和 lambda
      boxcox_result <- maxlikelihood_boxcox(quant_data, return_lambda = TRUE)
      quant_data <- boxcox_result$transformed_data
      boxcox_lambda <- boxcox_result$lambda
      
      # 检查转换后的数据是否有效
      if (any(is.na(quant_data)) || any(is.infinite(quant_data))) {
        stop("Box-Cox转换后数据包含无效值（NA或Inf），请检查原始数据")
      }
      
      # 确保转换后的数据都是正数（discretize_continuous要求正数）
      if (any(quant_data < 0, na.rm = TRUE)) {
        # 如果转换后有负数，进行平移使其变为正数
        min_val <- min(quant_data, na.rm = TRUE)
        if (min_val < 0) {
          boxcox_offset <- min_val - 0.1  # 记录平移量
          quant_data <- quant_data - boxcox_offset
        }
      }
    }
    
    # 使用自定义颗粒度进行离散化
    # grain_value 此时应该是数值（自定义的颗粒度）
    # 对于转换后的数据，使用支持小数颗粒度的离散化函数
    if (isTRUE(normalize_quantitative)) {
      # Box-Cox转换后的数据使用专门的离散化函数
      current_data$定量结果组 <- discretize_continuous(
        quant_data,
        grain = grain_value
      )
    } else {
      # 原始数据可以使用 discretize_age（假设数据范围合理）
      current_data$定量结果组 <- discretize_age(
        quant_data,
        grain = grain_value
      )
    }
    bin_col  <- "定量结果组"
    x_label  <- if (normalize_quantitative) "定量结果（Box-Cox转换后，横坐标为原值）" else "定量结果"
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
  
  # 如果进行了 Box-Cox 转换，需要将分组标签转换为原始值
  original_bin_labels <- NULL
  if (isTRUE(normalize_quantitative) && x_var == "定量结果" && !is.null(boxcox_lambda)) {
    # 获取当前的分组标签
    current_levels <- levels(plot_data[[bin_col]])
    
    # 解析标签，提取边界值，然后逆变换
    parse_and_reverse <- function(label) {
      # 标签格式通常是 "a-b" 或 "a-b.c"
      parts <- strsplit(label, "-")[[1]]
      if (length(parts) == 2) {
        lower_transformed <- as.numeric(parts[1])
        upper_transformed <- as.numeric(parts[2])
        
        # 如果有平移，先加上平移量
        # 注意：boxcox_offset 若为 NA，会导致 if 条件变 NA 从而报错
        if (!is.null(boxcox_offset) && is.finite(boxcox_offset) && !is.na(boxcox_offset) && boxcox_offset != 0) {
          lower_transformed <- lower_transformed + boxcox_offset
          upper_transformed <- upper_transformed + boxcox_offset
        }
        
        # 逆变换
        lower_original <- boxcox_reverse(lower_transformed, boxcox_lambda)
        upper_original <- boxcox_reverse(upper_transformed, boxcox_lambda)

        # 逆变换可能因为数值域问题产生 NA/NaN/Inf；此时保持原标签，避免 if(NA) 报错
        if (any(!is.finite(c(lower_original, upper_original))) || any(is.na(c(lower_original, upper_original)))) {
          return(label)
        }
        
        # 生成新标签，保留合理的小数位数
        max_abs <- suppressWarnings(max(abs(c(lower_original, upper_original)), na.rm = TRUE))
        if (!is.finite(max_abs) || is.na(max_abs)) {
          return(label)
        }
        if (max_abs > 1000) {
          # 大数值，保留较少小数位
          return(paste0(round(lower_original, 1), "-", round(upper_original, 1)))
        } else if (max_abs > 100) {
          # 中等数值，保留1位小数
          return(paste0(round(lower_original, 1), "-", round(upper_original, 1)))
        } else {
          # 小数值，保留2位小数
          return(paste0(round(lower_original, 2), "-", round(upper_original, 2)))
        }
      }
      return(label)  # 如果解析失败，返回原标签
    }
    
    # 为每个分组创建原始值标签
    original_bin_labels <- sapply(current_levels, parse_and_reverse)
    names(original_bin_labels) <- current_levels
  }

  list(
    plot_data = plot_data,
    color_vec = color_vec,
    bin_col   = bin_col,
    x_label   = x_label,
    ratio_col = ratio_col,
    original_bin_labels = original_bin_labels  # 原始值的分组标签（如果进行了Box-Cox转换）
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
    
    # 计算定量结果的默认颗粒度（range/40）
    default_quantitative_grain <- reactive({
      req(global_store[["filtered_data"]])
      data <- global_store[["filtered_data"]]
      if (!is.numeric(data[["定量结果"]])) {
        return(1)
      }
      quant_range <- range(data[["定量结果"]], na.rm = TRUE)
      default_grain <- (quant_range[2] - quant_range[1]) / 40
      print(default_grain)
      default_grain
    })
    
    # 当选择定量结果且需要正态化时，计算转换后的默认颗粒度
    default_normalized_quantitative_grain <- reactive({
      req(global_store[["filtered_data"]])
      req(input$x_var == "定量结果")
      
      data <- global_store[["filtered_data"]]
      if (!is.numeric(data[["定量结果"]])) {
        print("not_numeric")
        return(1)
      }
      
      # 进行Box-Cox转换（只需要转换后的数据，不需要 lambda）
      quant_data <- maxlikelihood_boxcox(data[["定量结果"]])
      quant_range <- range(quant_data, na.rm = TRUE)
      default_grain <- (quant_range[2] - quant_range[1]) / 40
      print(default_grain)
      default_grain
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
        # 获取当前的正态化状态（如果存在）
        normalize_val <- if (!is.null(input$normalize_quantitative)) {
          input$normalize_quantitative
        } else {
          FALSE
        }
        
        # 计算颗粒度默认值
        # 如果已经有 grain_value，优先保持它（避免在切换时重置）
        grain_default_val <- if (!is.null(input$grain_value) && is.numeric(input$grain_value) && !is.na(input$grain_value) && is.finite(input$grain_value)) {
          # 如果已经有值，保持它（observeEvent 会负责更新）
          input$grain_value
        } else {
          # 否则计算默认值
          tryCatch({
            if (isTRUE(normalize_val)) {
              round(default_normalized_quantitative_grain(), 2)
            } else {
              round(default_quantitative_grain(), 2)
            }
          }, error = function(e) {
            1  # 如果计算失败，返回默认值1
          })
        }
        
        tagList(
          checkboxInput(
            ns("normalize_quantitative"),
            label = "Box-Cox正态化",
            value = normalize_val,  # 使用当前值，而不是硬编码 FALSE
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
    # 使用 ignoreNULL = FALSE 确保即使值为 FALSE 也会触发
    # 使用 once = FALSE 允许多次触发（当用户切换时）
    observeEvent(input$normalize_quantitative, {
      # 只有当选择的是定量结果时才更新
      if (input$x_var != "定量结果") {
        return()
      }
      
      # 防止在控件初始化时触发
      if (is.null(input$normalize_quantitative)) {
        return()
      }
      
      new_value <- tryCatch({
        if (isTRUE(input$normalize_quantitative)) {
          default_normalized_quantitative_grain()
        } else {
          default_quantitative_grain()
        }
      }, error = function(e) {
        # 如果计算失败，保持当前值或使用默认值1
        if (!is.null(input$grain_value) && is.numeric(input$grain_value) && !is.na(input$grain_value) && is.finite(input$grain_value)) {
          input$grain_value
        } else {
          1
        }
      })
      new_value <- round(as.numeric(new_value), 2)
      
      # 只有当新值与当前值不同时才更新，避免不必要的更新
      if (!is.null(new_value) && is.numeric(new_value) && !is.na(new_value) && is.finite(new_value)) {
        current_value <- suppressWarnings(as.numeric(input$grain_value))
        if (is.null(current_value) || is.na(current_value) || !is.finite(current_value) || abs(current_value - new_value) > 0.001) {
          updateNumericInput(
            session,
            "grain_value",
            value = new_value
          )
        }
      }
    }, ignoreNULL = FALSE)

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

      # 统一过滤/提示：不同横坐标的颗粒度类型不同
      # - 年龄/定量结果：数值颗粒度
      # - 采样时间：字符颗粒度（day/week/month 或 日/周/月/年月日）
      grain_value_raw <- input$grain_value
      if (x_var == "采样时间") {
        grain_value <- as.character(grain_value_raw)
        allowed_time_grain <- c("day", "week", "month")
        shiny::validate(
          shiny::need(
            !is.null(grain_value) &&
              length(grain_value) == 1 &&
              nzchar(grain_value) &&
              grain_value %in% allowed_time_grain,
            message = "时间颗粒度必须是 day/week/month（或 日/周/月/年月日）。"
          )
        )
      } else {
        grain_value <- suppressWarnings(as.numeric(grain_value_raw))
        shiny::validate(
          shiny::need(
            !is.null(grain_value) &&
              length(grain_value) == 1 &&
              !is.na(grain_value) &&
              is.finite(grain_value) &&
              grain_value > 0,
            message = "颗粒度必须是一个有限的正数（例如 1、0.5、2.5）。"
          )
        )
      }
      
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
        grain_value             = grain_value,
        normalize_quantitative  = normalize_quantitative,
        order_para              = order_para
      )

      plot_data <- processed$plot_data
      bin_col   <- processed$bin_col
      x_label   <- processed$x_label
      ratio_col <- processed$ratio_col
      original_bin_labels <- processed$original_bin_labels

      # 如果进行了 Box-Cox 转换，使用原始值的标签
      if (!is.null(original_bin_labels) && length(original_bin_labels) > 0) {
        # 创建新的因子，使用原始值标签
        # 保持原有的顺序（levels）
        current_levels <- levels(plot_data[[bin_col]])
        new_levels <- original_bin_labels[current_levels]
        
        # 更新 plot_data 中的分组标签
        plot_data[[bin_col]] <- factor(
          plot_data[[bin_col]],
          levels = current_levels,
          labels = new_levels
        )
      }

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
      
      # 更新 x_label，如果使用了原始值标签
      if (!is.null(original_bin_labels) && length(original_bin_labels) > 0) {
        x_label <- "定量结果（Box-Cox转换后，横坐标为原值）"
      }

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
