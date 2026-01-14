#' 处理离散直方图数据
#'
#' @param data 数据框
#' @param primary_var 一级分层变量名 (string)
#' @param secondary_var 二级分层变量名 (string)
#' @param elements_num 保留前n个二级变量
#' @param sort_direction 排序方向系数 (通常为 1 或 -1)
#'
#' @return 处理后的 tibble
#' @noRd
#' @importFrom dplyr group_by summarise arrange mutate ungroup row_number if_else desc n pull distinct
#' @importFrom rlang sym !! :=
calc_discrete_hist_data <- function(data, primary_var, secondary_var, elements_num, sort_direction = 1) {

  # 步骤1: 数据聚合与排名
  processed_data <- data %>%
    group_by(!!sym(primary_var), !!sym(secondary_var)) %>%
    summarise(数量 = n(), .groups = "drop") %>%

    # 组内排序并标记Top N
    group_by(!!sym(primary_var)) %>%
    arrange(desc(数量), .by_group = TRUE) %>%
    mutate(
      rank = row_number(),
      second_dynamic_cat = if_else(
        rank <= elements_num,
        as.character(!!sym(secondary_var)),
        "其他"
      )
    ) %>%
    ungroup() %>%

    # 二次聚合（合并"其他"）
    group_by(!!sym(primary_var), second_dynamic_cat) %>%
    summarise(数量 = sum(数量), .groups = "drop") %>%

    # 计算统计指标
    group_by(!!sym(primary_var)) %>%
    mutate(总数 = sum(数量)) %>%
    ungroup() %>%
    mutate(比例 = 数量 / 总数) %>%

    # 最终排序 (注意：这里假设 sort_direction 用于控制二级排序逻辑)
    arrange(desc(总数), desc(sort_direction * 数量))

  # 步骤2: 调整因子水平，确保"其他"在最后
  # 获取所有非"其他"的类别并排序
  cats <- unique(processed_data$second_dynamic_cat)
  non_other_cats <- setdiff(cats, "其他")
  # 这里可以根据需要添加特定的字母排序逻辑，目前保持出现顺序

  processed_data <- processed_data %>%
    mutate(
      second_dynamic_cat = factor(
        second_dynamic_cat,
        levels = c(non_other_cats, "其他")
      )
    )

  # 步骤3: 确定一级变量的绘图顺序（按总数降序）
  main_cat_order <- processed_data %>%
    distinct(!!sym(primary_var), 总数) %>%
    arrange(desc(总数)) %>%
    dplyr::pull(!!sym(primary_var))

  processed_data <- processed_data %>%
    mutate(
      !!primary_var := factor(
        !!sym(primary_var),
        levels = main_cat_order
      )
    )

  return(list(
    data = processed_data,
    main_order = main_cat_order,
    non_other_cats = non_other_cats
  ))
}

#' 绘制离散堆叠直方图
#'
#' @param processed_list calc_discrete_hist_data 的返回结果
#' @param primary_var 一级变量名
#' @param secondary_var 二级变量名
#' @param plot_mode 绘图模式 ("数量" 或 "比例")
#' @param use_hash_color 是否使用哈希颜色 (TRUE/FALSE)
#'
#' @return plotly 对象
#' @noRd
#' @importFrom plotly plot_ly layout
#' @importFrom scales percent
plot_discrete_hist <- function(processed_list, primary_var, secondary_var, plot_mode, use_hash_color) {

  data <- processed_list$data
  main_order <- processed_list$main_order
  non_other_cats <- processed_list$non_other_cats

  # 颜色映射逻辑
  color_mapping <- NULL

  if (isTRUE(use_hash_color)) {
    # 假设 string_to_color 是外部定义的工具函数
    # 如果它是包内的函数，应该用 golem::get_golem_options 或直接调用 utils.R 中的函数
    color_palette <- unlist(lapply(non_other_cats, string_to_color))

    color_mapping <- setNames(
      c(color_palette, "grey"),
      c(non_other_cats, "其他")
    )
  }

  # 绘图
  p <- plot_ly(
    data = data,
    x = ~get(primary_var),
    y = ~get(plot_mode),
    color = ~second_dynamic_cat,
    colors = color_mapping,
    type = "bar",
    hovertext = ~paste0(
      "一级分层变量: ", get(primary_var), "<br>",
      "二级分层变量: ", second_dynamic_cat, "<br>",
      "数量: ", 数量, " (", scales::percent(比例, accuracy = 0.1), ")"
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    layout(
      barmode = "stack",
      xaxis = list(
        title = primary_var,
        categoryorder = "array",
        categoryarray = main_order
      ),
      yaxis = list(
        title = plot_mode,
        tickformat = ifelse(plot_mode == "比例", ".0%", "")
      ),
      legend = list(
        title = list(text = secondary_var)
      )
    )

  return(p)
}


#' qualitative_analysis_discrete_hist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qualitative_analysis_discrete_hist_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = TRUE,
    padding = 0,
    gap = "20px",
    class = "h-100",
    sidebar = sidebar(
      bg = "#f8f9fa",
      open = "desktop",
      uiOutput(ns("second_dynamic_Var_output")),

      selectInput(ns("sec_Var_mode"), "二级变量堆叠方式", choices = c("数量","比例")),
      sliderInput(ns("elements_num"), "上色前n个二级分层变量", min = 0, max = 100, value = 20)
    ),
    card(
      full_screen = TRUE,
      style = "aspect-ratio: 3/2; overflow: hidden;",
      card_body(
        padding = 0,
        plotly::plotlyOutput(ns("discrete_hist_output"), height = "100%", width = "100%")
      )
    )
  )
}

#' qualitative_analysis_discrete_hist Server Functions
#'
#' @noRd
mod_qualitative_analysis_discrete_hist_server <- function(id, global_store, header_for_stack, hash_color){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$second_dynamic_Var_output <- renderUI({
      print(0.25)
      req(global_store[["filtered_data"]])
      print(0.5)
      selectInput(ns("second_dynamic_Var"), "二级分层变量", choices = colnames(global_store[["filtered_data"]]))
    })

    output$discrete_hist_output <- plotly::renderPlotly({

      req(global_store[["filtered_data"]])
      order_para <- global_store[["order_para"]]
      # 1. 输入验证
      req(header_for_stack())
      req(input$second_dynamic_Var)

      data <- global_store[["filtered_data"]]

      validate(need(nrow(data) > 0, message = "没有可用数据"))
      validate(need(header_for_stack() != input$second_dynamic_Var, message = "一级/二级变量不能相同"))

      # 2. 调用业务逻辑函数处理数据
      # 注意：hash_color 是 reactive，所以用 hash_color()
      # input$sec_Var_mode, input$elements_num 直接使用

      processed_result <- calc_discrete_hist_data(
        data = data,
        primary_var = header_for_stack(),
        secondary_var = input$second_dynamic_Var,
        elements_num = input$elements_num,
        sort_direction = order_para
      )


      # 3. 调用绘图函数
      # 假设 hash_color() 返回的是字符串 "哈希颜色" 或其他
      use_hash <- (hash_color() == "哈希颜色")

      print(use_hash)

      p <- plot_discrete_hist(
        processed_list = processed_result,
        primary_var = header_for_stack(),
        secondary_var = input$second_dynamic_Var,
        plot_mode = input$sec_Var_mode,
        use_hash_color = use_hash
      )

      p

    }) %>% debounce(1000)

  })
}

## To be copied in the UI
# mod_qualitative_analysis_discrete_hist_ui("qualitative_analysis_discrete_hist_1")

## To be copied in the server
# mod_qualitative_analysis_discrete_hist_server("qualitative_analysis_discrete_hist_1")
