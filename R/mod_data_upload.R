#' data_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr %>%
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "数据上传",
    fileInput(
      inputId = ns("Input_data"),
      label = "请上传需要分析的数据（Excel文件）",
      multiple = TRUE,
      accept = c(".xlsx"),
      width = "100%"
    ),
    actionButton(
      ns("read_data"),
      "确认上传数据",
      icon = icon("upload"),
      # 逻辑：先通过变量 el 获取元素，然后清空类名，再修改 HTML
      onclick = sprintf(
        "var el = document.getElementById('%s'); el.className = ''; el.innerHTML = '<span class=\"stat-loading\"><i class=\"fa fa-spinner fa-spin\"></i> 正在上传数据，请稍候...</span>';",
        ns("status_box")
      )
    ),
    uiOutput(ns("data_upload_stat"))
  )
}

#' data_upload Server Functions
#'
#' @noRd
mod_data_upload_server <- function(id, global_store, header_for_stack, hash_color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observeEvent(input$read_data, {
      # 1. 安全检查：确保有文件
      req(input$Input_data)

      df <- tryCatch({
        input$Input_data$datapath %>%
          get_data() %>%
          do.call(rbind, .)
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
        return(NULL)
      })

      req(df) # 如果读取失败则中断

      # 数据清洗：将所有 NA 转化为字符串 "NA"
      # 这里也是阻塞点
      df <- as.data.frame(
        lapply(df, function(col) {
          col = as.character(col) %>% replace(is.na(.), "NA")
        }),
        stringsAsFactors = FALSE
      )

      # 更新全局状态总线
      global_store[["reactive_data_na_"]] <- df

      # 计算结束：推送“成功”状态覆盖前端的临时状态
      output$data_upload_stat <- renderUI({
        div(
          id = ns("status_box"),
          class = "stat-success",
          icon("check-circle"),
          " 数据已成功上传"
        )
      })
    })

    output$data_upload_stat <- renderUI({
      div(
        id = ns("status_box"),
        class = "stat-warning",
        "请上传数据"
      )
    })

  })
}

## To be copied in the UI
# mod_data_upload_ui("data_upload_1")

## To be copied in the server
# mod_data_upload_server("data_upload_1")
