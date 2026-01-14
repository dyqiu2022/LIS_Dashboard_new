#' analysis_tools_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib accordion accordion_panel
#' @importFrom dplyr %>% filter mutate
#' @importFrom shinyWidgets pickerInput
mod_analysis_tools_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("abnormal_result_filters")),
    uiOutput(ns("filters")),
    uiOutput(ns("download_html_ui")),
  )
}

#' analysis_tools_sidebar Server Functions
#'
#' @noRd
mod_analysis_tools_sidebar_server <- function(id, global_store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$abnormal_result_filters <- renderUI({
      req(global_store[["reactive_data_na_"]])

      # 使用 accordion 代替 wellPanel
      bslib::accordion(
        id = ns("filter_accordion"),
        open = FALSE, # 默认收起，点击再展开
        bslib::accordion_panel(
          value = "missing_value_panel",
          title = span(icon("filter"), "缺失/无效值筛选器"),
          # 这里放你的 checkbox
          checkboxInput(ns("invalid_date_filter"), "过滤无效日期", value = TRUE),
          checkboxInput(ns("invalid_age_filter"), "过滤无效年龄", value = TRUE),
          checkboxInput(ns("invalid_result_filter"), "过滤无效定量结果", value = TRUE)
        )
      )
    })

    reactive_data <- reactive({
      req(global_store[["reactive_data_na_"]])

      validate(need(nrow(global_store[["reactive_data_na_"]])>0, message = ""))
      validate(need(!is.null(input$invalid_date_filter), message = ""))
      validate(need(!is.null(input$invalid_age_filter), message = ""))
      validate(need(!is.null(input$invalid_result_filter), message = ""))

      na_results <- c("结果无效", "NA")
      data <- global_store[["reactive_data_na_"]]
      if (input$invalid_date_filter == TRUE){
        data <- data %>% dplyr::filter(!(采样时间 %in% na_results))
        data <- data %>% dplyr::mutate(采样时间 = safe_date_convert(采样时间))
      }
      if (input$invalid_age_filter == TRUE){
        data <- data %>% dplyr::filter(!(年龄 %in% na_results))
        data <- data %>% dplyr::mutate(年龄 = as.numeric(年龄))
      }
      if (input$invalid_result_filter == TRUE){
        data <- data %>% dplyr::filter(!(定量结果 %in% na_results))
        data <- data %>% dplyr::mutate(定量结果 = as.numeric(定量结果))
      }
      data
    }) %>% debounce(1000)

    # 将reactive_data同步至数据总线
    observe({
      global_store[["reactive_data"]] <- reactive_data()
    })

    output$filters <- renderUI({
      req(global_store[["reactive_data"]])
      validate(
        need(
          nrow(global_store[["reactive_data"]])>0,
          message = "当前数据集中剔除NA值后无数据"
        )
      )
      df <- global_store[["reactive_data"]]
      posi_headers <- colnames(df)
      wellPanel(
        h5("变量筛选器"),
        lapply(posi_headers, function(col) {
          values <- unique(df[[col]])
          n_unique <- length(values)
          if (length(unique(df[[col]])) %in% c(1, 0)){
            return(NULL)
          }
          if (is.numeric(df[[col]])) {
            sliderInput(
              inputId = ns(paste0("filter_", col)),
              label = col,
              min = floor(min(df[[col]] %>% na.omit())),
              max = ceiling(max(df[[col]] %>% na.omit())),
              value = range(df[[col]] %>% na.omit())
            )
          } else {
            if (col=="采样时间" && input$invalid_date_filter == TRUE){
              dateRangeInput(
                ns(paste0("filter_", col)),
                "选择采样时间范围:",
                start = min(global_store[["reactive_data"]]$采样时间),
                end = max(global_store[["reactive_data"]]$采样时间),
                language = "zh-CN"
              )
            } else if (n_unique > 200) {
              return(NULL)
            } else {
              # 普通列使用常规选择器
              pickerInput(
                inputId = ns(paste0("filter_", col)),
                label = col,
                choices = values,
                selected = values,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              )
            }
          }
        })
      )
    })

    # 响应式数据过滤
    filtered_data <- reactive({
      req(global_store[["reactive_data"]])
      data <- global_store[["reactive_data"]]

      # --- 1. 核心修复：年龄过滤的安全检查 ---
      age_input <- input[["filter_年龄"]]
      if (input$invalid_age_filter == TRUE && !is.null(age_input)) {
        # 确保 age_input 长度为 2 (range slider)
        data <- data %>% filter(
          年龄 >= age_input[1],
          年龄 <= age_input[2]
        )
      } else if (!is.null(age_input)) {
        # 针对离散选择的过滤
        data <- data %>% filter(年龄 %in% age_input)
      }

      # --- 2. 定量结果过滤的安全检查 ---
      res_input <- input[["filter_定量结果"]]
      if (input$invalid_result_filter == TRUE && !is.null(res_input)) {
        data <- data %>% filter(
          定量结果 >= res_input[1],
          定量结果 <= res_input[2]
        )
      } else if (!is.null(res_input)) {
        data <- data %>% filter(定量结果 %in% res_input)
      }

      # --- 3. 采样时间过滤的安全检查 ---
      date_input <- input[["filter_采样时间"]]
      if (input$invalid_date_filter == TRUE && !is.null(date_input)) {
        data <- data %>% filter(
          采样时间 >= date_input[1],
          采样时间 <= date_input[2]
        )
      }

      # --- 4. 自动化处理剩余的普通分类变量 (工业级写法) ---
      # 定义你需要过滤的分类列名列表
      cat_cols <- c(
        "医院名称", "开单科室", "病人类型", "样本类型", "试剂厂家",
        "方法学", "项目名称", "项目序号", "性别", "临床诊断",
        "备注信息", "定性结果", "类别_关键词", "类别_无监督",
        "定量结果_离散", "年龄_离散"
      )

      for (col in cat_cols) {
        val <- input[[paste0("filter_", col)]]
        # 关键检查：只有当输入不为 NULL 且长度大于 0 时才过滤
        if (!is.null(val) && length(val) > 0) {
          # 使用 rlang 的注入方式动态过滤列名
          data <- data %>% dplyr::filter(.data[[col]] %in% val)
        }
      }

      return(data)
    }) %>% debounce(1000)

    # 将reactive_data同步至数据总线
    observe({
      global_store[["filtered_data"]] <- filtered_data()
    })

  })
}

## To be copied in the UI
# mod_analysis_tools_sidebar_ui("analysis_tools_sidebar_1")

## To be copied in the server
# mod_analysis_tools_sidebar_server("analysis_tools_sidebar_1")
