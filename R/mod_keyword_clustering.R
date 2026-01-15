#' keyword_clustering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom dplyr %>% arrange group_by reframe select
#' @importFrom rlang sym .data
mod_keyword_clustering_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "关键词聚类",
    fluidRow(
      column(2, uiOutput(ns("choose_grouping_col")))
    ),
    fluidRow(
      style = "height: 1500px; display: flex;",
      column(
        6,
        style = "display: flex; flex-direction: column; height: 80%;",
        div(
          style = "flex: 1; min-height: 0;",
          DT::dataTableOutput(ns("diag_count"), width = "100%")
        )
      ),
      column(
        6,
        style = "display: flex; flex-direction: column; height: 100%;",
        div(
          style = "flex: 1; min-height: 0; display: flex; flex-direction: column;",
          fluidRow(
            column(
              12,
              h4("关键词聚类条件输入", style = "text-align: center;"),
              div(style = "flex: 1; overflow: auto;", uiOutput(ns("word_grouping_ui")))
            )
          )
        ),
        div(
          style = "flex: 1; min-height: 0;",
          fluidRow(
            column(
              12,
              h4("可视化结果", style = "text-align: center;"),
              div(style = "height: 100%;", plotly::plotlyOutput(ns("visualization_plot"), height = "100%"))
            )
          )
        )
      )
    )
  )
}

#' keyword_clustering Server Functions
#'
#' @noRd
mod_keyword_clustering_server <- function(id, global_store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    and_lst_ <- reactiveVal(list())
    not_lst_ <- reactiveVal(character(0))
    not_limit_lst_ <- reactiveVal(character(0))

    grouping_table <- reactiveVal(NULL)

    Saved_Class_name <- reactiveVal("")
    Saved_And1 <- reactiveVal("")
    Saved_And2 <- reactiveVal("")
    Saved_And3 <- reactiveVal("")
    Saved_Not <- reactiveVal("")
    Saved_Not_limit <- reactiveVal("")
    Saved_selected_exclude_group <- reactiveVal(character(0))

    parse_and_conditions <- function(and1, and2, and3) {
      lst <- lapply(list(and1, and2, and3), function(x) {
        if (is.null(x) || is.na(x)) return(character(0))
        base::unlist(base::strsplit(x, split = "[；;|]"))
      })
      lst <- lapply(lst, function(x) base::trimws(x[x != ""]))
      lst[lengths(lst) > 0]
    }

    parse_simple_list <- function(x) {
      if (is.null(x) || is.na(x)) return(character(0))
      out <- base::unlist(base::strsplit(x, split = "[；;|]"))
      out <- base::trimws(out)
      out[out != ""]
    }

    observe({
      req(global_store[["reactive_data_na_"]])
      validate(need(nrow(global_store[["reactive_data_na_"]]) > 0, message = ""))
      req(input$selected_grouping_col)

      data <- global_store[["reactive_data_na_"]]
      if (!"类别_关键词" %in% colnames(data)) data$`类别_关键词` <- ""

      new_table <- data %>%
        dplyr::group_by(!!rlang::sym(input$selected_grouping_col), `类别_关键词`) %>%
        dplyr::reframe(数量 = dplyr::n()) %>%
        dplyr::arrange(-数量)

      grouping_table(new_table)
    })

    output$diag_count <- DT::renderDataTable({
      req(grouping_table())
      DT::datatable(
        grouping_table(),
        extensions = c("Buttons"),
        options = list(
          pageLength = 40,
          dom = "Blfrtip",
          paging = TRUE,
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          autoWidth = TRUE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/zh.json"),
          paginate = list(first = "首页", previous = "上一页", `next` = "下一页", last = "末页")
        ),
        rownames = FALSE,
        filter = "top"
      )
    })

    output$choose_grouping_col <- renderUI({
      req(global_store[["reactive_data_na_"]])
      cols <- colnames(global_store[["reactive_data_na_"]])
      choices <- cols[cols != "类别_关键词"]
      selected <- if ("临床诊断" %in% choices) "临床诊断" else choices[[1]]
      selectInput(ns("selected_grouping_col"), "关键词聚类列", choices = choices, selected = selected, width = "100%")
    })

    output$word_grouping_ui <- renderUI({
      req(grouping_table())
      exclude_choices <- colnames(grouping_table())
      exclude_choices <- exclude_choices[!exclude_choices %in% c(input$selected_grouping_col, "类别_关键词", "数量")]

      wellPanel(
        fluidRow(
          column(
            4,
            textInput(
              ns("Class_name"),
              label = tagList(icon("tag"), " 类别名称", span("*", style = "color: red;")),
              width = "100%",
              value = Saved_Class_name()
            )
          ),
          column(
            4,
            textInput(
              ns("And1"),
              label = tagList(icon("filter"), " 且条件1", span("*", style = "color: red;")),
              placeholder = "用 | 分隔多个条件",
              width = "100%",
              value = Saved_And1()
            )
          ),
          column(
            4,
            textInput(
              ns("And2"),
              label = tagList(icon("filter"), " 且条件2"),
              placeholder = "可选附加条件",
              width = "100%",
              value = Saved_And2()
            )
          )
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("And3"),
              label = tagList(icon("filter"), " 且条件3"),
              placeholder = "可选附加条件",
              width = "100%",
              value = Saved_And3()
            )
          ),
          column(
            4,
            textInput(
              ns("Not"),
              label = tagList(icon("ban"), " 非条件"),
              placeholder = "排除满足这些条件的记录",
              width = "100%",
              value = Saved_Not()
            )
          ),
          column(
            4,
            textInput(
              ns("Not_limit"),
              label = tagList(" 限制条件"),
              placeholder = "对非条件的适用范围做出限定",
              width = "100%",
              value = Saved_Not_limit()
            )
          )
        ),
        fluidRow(
          column(
            12,
            selectizeInput(
              ns("exclude_group"),
              label = tagList(icon("link-slash"), " 互斥类别"),
              choices = exclude_choices,
              multiple = TRUE,
              options = list(placeholder = "选择需要排除的类别", plugins = list("remove_button")),
              selected = Saved_selected_exclude_group(),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(8, textOutput(ns("words_sentance")) %>% tagAppendAttributes(style = "color: #4DAF4A; font-weight: 500;")),
          column(2, actionButton(ns("start_grouping"), "创建新类", width = "100%")),
          column(2, actionButton(ns("over_write_data"), "写入数据", width = "100%"))
        )
      )
    })

    output$words_sentance <- renderText({
      and_lst <- parse_and_conditions(input$And1, input$And2, input$And3)
      and_counter <- 0
      note_sentance <- ""

      for (i in and_lst) {
        and_counter <- and_counter + 1
        and_connect <- ifelse(and_counter == 1, "：包含(\"", ", 且包含(\"")
        note_sentance <- paste0(note_sentance, and_connect)
        note_sentance <- paste0(note_sentance, paste(i, collapse = "\"或\""))
        note_sentance <- paste0(note_sentance, "\")")
      }

      if ("ALL" %in% unlist(and_lst)) note_sentance <- "：全部数据"

      not_lst <- parse_simple_list(input$Not)
      not_limit_lst <- parse_simple_list(input$Not_limit)

      if (length(not_lst) > 0) {
        note_sentance <- paste(note_sentance, ", 不包含(\"")
        note_sentance <- paste0(note_sentance, paste(not_lst, collapse = "\"或\""))
        if (length(not_limit_lst) > 0) {
          note_sentance <- paste0(note_sentance, "\", 除非包含\"", paste(not_limit_lst, collapse = "\"或\""))
        }
        note_sentance <- paste0(note_sentance, "\")")
      }

      if (length(input$exclude_group) > 0) {
        note_sentance <- paste(note_sentance, ", 与(\"")
        note_sentance <- paste0(note_sentance, paste(input$exclude_group, collapse = "\", \""))
        note_sentance <- paste0(note_sentance, "\")互斥")
      }

      and_lst_(and_lst)
      not_lst_(not_lst)
      not_limit_lst_(not_limit_lst)

      paste0(input$Class_name, note_sentance)
    })

    observeEvent(input$start_grouping, {
      req(grouping_table())
      req(input$selected_grouping_col)
      req(input$Class_name)

      grouping_info_vec <- grouping_table()[[input$selected_grouping_col]] %>% unlist()
      grouping_num <- lapply(
        grouping_info_vec,
        grouping_muti_groups,
        list_condition = and_lst_(),
        not = not_lst_(),
        not_restric = not_limit_lst_()
      ) %>%
        unlist() %>%
        which()

      for (i in input$exclude_group) {
        excluded_num <- which(unlist(grouping_table()[[i]]) != "")
        grouping_num <- grouping_num[!(grouping_num %in% excluded_num)]
      }

      new_grouping_table <- grouping_table()
      new_grouping_table[[input$Class_name]] <- ""
      new_grouping_table[grouping_num, ][[input$Class_name]] <- input$Class_name

      Saved_Class_name(input$Class_name)
      Saved_And1(input$And1)
      Saved_And2(input$And2)
      Saved_And3(input$And3)
      Saved_Not(input$Not)
      Saved_Not_limit(input$Not_limit)
      Saved_selected_exclude_group(input$exclude_group)

      grouping_table(new_grouping_table)
    })

    observeEvent(input$over_write_data, {
      req(global_store[["reactive_data_na_"]])
      req(grouping_table())
      req(input$selected_grouping_col)

      merge_table2 <- grouping_table()[, c(which(!colnames(grouping_table()) %in% c(input$selected_grouping_col, "类别_关键词", "数量")))] %>%
        apply(1, function(i) {
          unlist(i)[which(unlist(i) != "")] %>% paste(collapse = "|")
        }) %>%
        cbind(., grouping_table()[[input$selected_grouping_col]])

      colnames(merge_table2) <- c("类别_关键词", input$selected_grouping_col)
      new_reactive_data <- global_store[["reactive_data_na_"]] %>% dplyr::select(-dplyr::any_of("类别_关键词"))
      saved_col_names <- colnames(new_reactive_data)
      new_reactive_data <- merge(new_reactive_data, merge_table2, by = input$selected_grouping_col, all.x = TRUE)
      new_reactive_data <- new_reactive_data[, c(saved_col_names, "类别_关键词")]

      if (length(which(new_reactive_data$`类别_关键词` == "")) > 0) {
        new_reactive_data[which(new_reactive_data$`类别_关键词` == ""), ]$`类别_关键词` <- "未知"
      }

      global_store[["reactive_data_na_"]] <- new_reactive_data
    })

    output$visualization_plot <- plotly::renderPlotly({
      req(grouping_table())
      tbl <- grouping_table()

      class_cols <- colnames(tbl)
      class_cols <- class_cols[!class_cols %in% c(input$selected_grouping_col, "类别_关键词", "数量")]

      if (length(class_cols) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "暂无可视化：请先创建新类"))
      }

      counts <- vapply(class_cols, function(col) sum(tbl[[col]] != "", na.rm = TRUE), FUN.VALUE = numeric(1))
      df_plot <- data.frame(类别 = names(counts), 数量 = as.numeric(counts), check.names = FALSE)
      df_plot <- df_plot %>% dplyr::arrange(dplyr::desc(.data$数量))

      plotly::plot_ly(df_plot, x = ~类别, y = ~数量, type = "bar") %>%
        plotly::layout(
          title = "关键词聚类：各类别命中数量",
          xaxis = list(title = "类别", tickangle = -45),
          yaxis = list(title = "数量"),
          margin = list(b = 120)
        )
    })
  })
}

