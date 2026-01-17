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
#' @importFrom DT renderDataTable datatable formatStyle dataTableOutput
#' @importFrom bslib card_body
#' @importFrom shiny validate need
mod_keyword_clustering_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "关键词聚类",
    fillable = TRUE,
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "聚类参数与条件输入",
        width = "360px",
        bg = "#f8f9fa",
        open = "desktop",
        uiOutput(ns("choose_grouping_col")),
        div(class = "pt-3", uiOutput(ns("word_grouping_ui")))
      ),
      layout_column_wrap(
        width = 1 / 2,
        height = "100%",
        fill = TRUE,
        gap = "20px",
        card(
          style = "overflow: hidden; height: 100%;",
          full_screen = TRUE,
          card_header("聚类统计表格"),
          card_body(
            padding = 0,
            DT::dataTableOutput(ns("diag_count"), height = "100%", fill = TRUE)
          )
        ),
        card(
          style = "overflow: hidden; height: 100%;",
          full_screen = TRUE,
          card_header("可视化结果"),
          div(class = "p-2", style = "height: calc(100% - 60px);", plotly::plotlyOutput(ns("visualization_plot"), height = "100%"))
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
    # 追踪已创建但尚未"写入数据"的类（用于可视化）
    pending_classes <- reactiveVal(character(0))

    # 存储每个类的定义信息（用于回溯）
    class_definitions <- reactiveValues()

    # 当前显示的类的定义（用于控件默认值）
    Current_Class_name <- reactiveVal("")
    Current_And1 <- reactiveVal("")
    Current_And2 <- reactiveVal("")
    Current_And3 <- reactiveVal("")
    Current_Not <- reactiveVal("")
    Current_Not_limit <- reactiveVal("")
    Current_selected_exclude_group <- reactiveVal(character(0))

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

      # 先按临床诊断和类别_关键词分组统计
      new_table <- data %>%
        dplyr::group_by(!!rlang::sym(input$selected_grouping_col), `类别_关键词`) %>%
        dplyr::reframe(临床诊断数量 = dplyr::n()) %>%
        dplyr::arrange(-临床诊断数量)

      # 再按类别_关键词汇总，计算类别数量
      category_counts <- new_table %>%
        dplyr::group_by(`类别_关键词`) %>%
        dplyr::reframe(类别数量 = sum(临床诊断数量, na.rm = TRUE)) %>%
        dplyr::arrange(-类别数量)

      # 合并成4列：临床诊断、临床诊断数量、类别_关键词、类别数量
      new_table <- merge(new_table, category_counts, by = "类别_关键词", all.x = TRUE)
      new_table <- new_table[, c(input$selected_grouping_col, "临床诊断数量", "类别_关键词", "类别数量")]
      new_table <- new_table %>% dplyr::arrange(-类别数量, -临床诊断数量)

      grouping_table(new_table)
    })

    output$diag_count <- DT::renderDataTable({
      req(grouping_table())
      data <- grouping_table()
      validate(need(nrow(data) > 0, "暂无聚类数据"))
      
      DT::datatable(
        data,
        extensions = c('Buttons'),
        fillContainer = TRUE,
        style = "bootstrap",
        class = "table table-striped table-hover table-sm display nowrap",
        rownames = FALSE,
        selection = 'none',
        filter = 'top',
        options = list(
          paging = TRUE,
          scroller = FALSE,
          pageLength = 20,
          lengthMenu = c(15, 20, 50, 100),
          dom = '<"d-flex justify-content-between mb-2"Bf>t<"d-flex justify-content-between align-items-center mt-2"ilp>',
          buttons = list(
            list(
              extend = 'colvis',
              text = '显示/隐藏列',
              columns = ':not(:first-child)',
              className = 'btn-sm btn-outline-secondary'
            )
          ),
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json',
            buttons = list(colvis = '列可见性')
          )
        )
      ) %>%
        DT::formatStyle(
          columns = names(data),
          fontSize = '14px',
          lineHeight = '20px'
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
      req(input$selected_grouping_col)

      # 获取互斥类别选项（排除基础列和当前选中的分组列）
      exclude_choices <- colnames(grouping_table())
      exclude_choices <- exclude_choices[!exclude_choices %in% c(input$selected_grouping_col, "类别_关键词", "临床诊断数量", "类别数量")]

      pending <- pending_classes()
      restore_choices <- c("", pending)

      tagList(
        # 回溯功能：选择待写入的类
        div(
          class = "pb-2",
          selectInput(
            ns("restore_class"),
            "回溯/编辑类",
            choices = restore_choices,
            selected = if (is.null(input$restore_class)) "" else input$restore_class,
            width = "100%"
          )
        ),
        div(
          class = "pb-2",
          actionButton(
            ns("delete_selected_class"),
            "删除选中类",
            icon = icon("trash"),
            class = "btn-danger",
            width = "100%"
          )
        ),
        hr(),
        # 条件输入：每个占一行
        textInput(
          ns("Class_name"),
          label = tagList(icon("tag"), " 类别名称", span("*", style = "color: red;")),
          width = "100%",
          value = Current_Class_name()
        ),
        textInput(
          ns("And1"),
          label = tagList(icon("filter"), " 且条件1", span("*", style = "color: red;")),
          placeholder = "用 | 分隔多个条件",
          width = "100%",
          value = Current_And1()
        ),
        textInput(
          ns("And2"),
          label = tagList(icon("filter"), " 且条件2"),
          placeholder = "可选附加条件",
          width = "100%",
          value = Current_And2()
        ),
        textInput(
          ns("And3"),
          label = tagList(icon("filter"), " 且条件3"),
          placeholder = "可选附加条件",
          width = "100%",
          value = Current_And3()
        ),
        textInput(
          ns("Not"),
          label = tagList(icon("ban"), " 非条件"),
          placeholder = "排除满足这些条件的记录",
          width = "100%",
          value = Current_Not()
        ),
        textInput(
          ns("Not_limit"),
          label = tagList(" 限制条件"),
          placeholder = "对非条件的适用范围做出限定",
          width = "100%",
          value = Current_Not_limit()
        ),
        div(class = "pt-2", selectizeInput(
          ns("exclude_group"),
          label = tagList(icon("link-slash"), " 互斥类别"),
          choices = exclude_choices,
          multiple = TRUE,
          options = list(placeholder = "选择需要排除的类别", plugins = list("remove_button")),
          selected = Current_selected_exclude_group(),
          width = "100%"
        )),
        div(class = "pt-2", textOutput(ns("words_sentance")) %>% tagAppendAttributes(style = "color: #4DAF4A; font-weight: 500;")),
        div(class = "pt-2", actionButton(ns("start_grouping"), "创建新类", width = "100%")),
        div(class = "pt-2", actionButton(ns("over_write_data"), "写入数据", width = "100%"))
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

      # 将新创建的类加入待写入列表
      current_pending <- pending_classes()
      if (!input$Class_name %in% current_pending) {
        pending_classes(c(current_pending, input$Class_name))
      }

      # 保存类的定义信息（用于回溯）
      class_definitions[[input$Class_name]] <- list(
        Class_name = input$Class_name,
        And1 = input$And1,
        And2 = input$And2,
        And3 = input$And3,
        Not = input$Not,
        Not_limit = input$Not_limit,
        exclude_group = input$exclude_group
      )

      # 更新当前显示的类的定义
      Current_Class_name(input$Class_name)
      Current_And1(input$And1)
      Current_And2(input$And2)
      Current_And3(input$And3)
      Current_Not(input$Not)
      Current_Not_limit(input$Not_limit)
      Current_selected_exclude_group(input$exclude_group)

      grouping_table(new_grouping_table)
    })

    # 回溯功能：选择待写入的类后恢复其定义
    observeEvent(input$restore_class, {
      if (!is.null(input$restore_class) && input$restore_class != "") {
        if (input$restore_class %in% names(class_definitions)) {
          def <- class_definitions[[input$restore_class]]
          Current_Class_name(def$Class_name)
          Current_And1(def$And1)
          Current_And2(def$And2)
          Current_And3(def$And3)
          Current_Not(def$Not)
          Current_Not_limit(def$Not_limit)
          Current_selected_exclude_group(def$exclude_group)
        }
      } else {
        # 清空控件
        Current_Class_name("")
        Current_And1("")
        Current_And2("")
        Current_And3("")
        Current_Not("")
        Current_Not_limit("")
        Current_selected_exclude_group(character(0))
      }
    })

    # 删除选中类
    observeEvent(input$delete_selected_class, {
      req(input$restore_class)
      if (input$restore_class != "" && input$restore_class %in% pending_classes()) {
        # 从 grouping_table 中删除该列
        tbl <- grouping_table()
        if (input$restore_class %in% colnames(tbl)) {
          tbl[[input$restore_class]] <- NULL
          grouping_table(tbl)
        }

        # 从 pending_classes 中移除
        current_pending <- pending_classes()
        current_pending <- current_pending[current_pending != input$restore_class]
        pending_classes(current_pending)

        # 从 class_definitions 中移除
        class_definitions[[input$restore_class]] <- NULL

        # 清空选中和控件
        updateSelectInput(session, "restore_class", selected = "")
        Current_Class_name("")
        Current_And1("")
        Current_And2("")
        Current_And3("")
        Current_Not("")
        Current_Not_limit("")
        Current_selected_exclude_group(character(0))
      }
    })

    observeEvent(input$over_write_data, {
      req(global_store[["reactive_data_na_"]])
      req(grouping_table())
      req(input$selected_grouping_col)

      merge_table2 <- grouping_table()[, c(which(!colnames(grouping_table()) %in% c(input$selected_grouping_col, "类别_关键词", "临床诊断数量", "类别数量")))] %>%
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

      # 清空待写入列表（已写入，不再可视化）
      pending_classes(character(0))
    })

    output$visualization_plot <- plotly::renderPlotly({
      req(grouping_table())
      req(input$selected_grouping_col)

      pending <- pending_classes()
      if (length(pending) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(
          title = "暂无可视化：请先创建新类",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ))
      }

      tbl <- grouping_table()
      # 检查表格是否为空
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(
          title = "暂无可视化：表格数据为空",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ))
      }

      # 检查分组列是否存在
      if (!input$selected_grouping_col %in% colnames(tbl)) {
        return(plotly::plot_ly() %>% plotly::layout(
          title = paste0("错误：分组列 '", input$selected_grouping_col, "' 不存在"),
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ))
      }

      # 筛选出已创建但未写入的类的数据
      plot_data_list <- list()
      for (class_name in pending) {
        # 检查该列是否存在
        if (!class_name %in% colnames(tbl)) next

        # 筛选出该类别非空的行（即有数据被分配到该类）
        class_col <- tbl[[class_name]]
        if (is.null(class_col)) next

        class_rows <- which(class_col != "" & !is.na(class_col))
        if (length(class_rows) > 0) {
          class_data <- tbl[class_rows, , drop = FALSE]
          if (nrow(class_data) > 0) {
            for (i in seq_len(nrow(class_data))) {
              # 安全获取诊断名称
              diag_name <- if (input$selected_grouping_col %in% colnames(class_data) && 
                               !is.na(class_data[[input$selected_grouping_col]][i])) {
                as.character(class_data[[input$selected_grouping_col]][i])
              } else {
                "未知"
              }
              
              # 安全获取数量：如果列不存在或值为NA，使用1作为默认值
              count_val <- if ("临床诊断数量" %in% colnames(class_data) && 
                               !is.na(class_data[["临床诊断数量"]][i])) {
                val <- suppressWarnings(as.numeric(class_data[["临床诊断数量"]][i]))
                if (is.na(val) || !is.finite(val)) 1 else val
              } else {
                1
              }
              
              plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
                关键词类 = class_name,
                临床诊断 = diag_name,
                数量 = count_val,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }

      if (length(plot_data_list) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(
          title = "暂无可视化：已创建的类暂无数据",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ))
      }

      df_plot <- do.call(rbind, plot_data_list)
      # 按关键词类和临床诊断汇总（如果有重复需要累加）
      df_plot <- df_plot %>%
        dplyr::group_by(关键词类, 临床诊断) %>%
        dplyr::reframe(数量 = sum(数量, na.rm = TRUE)) %>%
        dplyr::arrange(关键词类, dplyr::desc(数量))

      # 获取所有临床诊断，用于颜色映射
      all_diags <- unique(df_plot$临床诊断)
      if (length(all_diags) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(
          title = "暂无可视化：已创建的类暂无有效数据",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ))
      }

      # 生成颜色（参考 qualitative_analysis 的风格）
      # string_to_color 只接受单个字符串，需要对向量中的每个元素调用
      color_map <- stats::setNames(
        sapply(all_diags, function(diag) string_to_color(diag, saturation = 0.6, lightness = 0.6)),
        all_diags
      )

      # 创建堆叠直方图
      p <- plotly::plot_ly()
      for (diag in all_diags) {
        diag_data <- df_plot %>% dplyr::filter(临床诊断 == diag)
        if (nrow(diag_data) > 0) {
          p <- p %>% plotly::add_trace(
            data = diag_data,
            x = ~关键词类,
            y = ~数量,
            type = "bar",
            name = diag,
            marker = list(color = color_map[[diag]]),
            hovertemplate = paste0(
              "关键词类: %{x}<br>",
              "临床诊断: ", diag, "<br>",
              "数量: %{y}<extra></extra>"
            )
          )
        }
      }

      p %>% plotly::layout(
        title = "关键词聚类可视化（堆叠直方图）",
        barmode = "stack",
        xaxis = list(title = "关键词类", tickangle = -45),
        yaxis = list(title = "数量"),
        showlegend = FALSE,
        margin = list(b = 120)
      )
    })
  })
}

