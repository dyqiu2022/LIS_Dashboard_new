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
    title = "数值变量离散化",
    fillable = TRUE,
    # 用页面级 sidebar 承载“离散化参数”，避免顶部区域被挤压/不可滚动导致不可见
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "离散化参数",
        width = "360px",
        bg = "#f8f9fa",
        open = "desktop",
        selectInput(
          ns("chosed_continuous_col"),
          "离散化变量",
          choices = character(0),
          selected = NULL,
          width = "100%"
        ),
        textInput(
          ns("cut_points"),
          "输入分割点（`|` 分隔）",
          placeholder = "如：20|50|80|100 或 5%|30%|60%|95%",
          value = "5%|30%|50%|70%|95%",
          width = "100%"
        ),
        actionButton(ns("cut_continuous_data"), "确认分割，覆盖数据", width = "100%"),
        div(
          class = "pt-2",
          textOutput(ns("text_group_names")) %>%
            tagAppendAttributes(style = "color: #4DAF4A; font-weight: 500;")
        )
      ),
      # 右侧：四个版块（2x2）
      tagList(
        layout_column_wrap(
          width = 1 / 2,
          height = NULL,
          fill = FALSE,
          gap = "20px",
          card(
            style = "aspect-ratio: 5/ 3; overflow: hidden;",
            full_screen = TRUE,
            layout_sidebar(
              fillable = TRUE,
              padding = 8,
              gap = "12px",
              sidebar = sidebar(
                title = "图表参数",
                width = "150px",
                bg = "#f8f9fa",
                open = "desktop",
                selectInput(
                  ns("select_transform"),
                  "正态变换",
                  choices = c("原数据", "Box-Cox"),
                  selected = "原数据",
                  width = "100%"
                ),
                numericInput(
                  ns("bin_num"),
                  "直方数量",
                  min = 1,
                  max = 999999,
                  value = 50,
                  width = "100%"
                )
              ),
              plotly::plotlyOutput(ns("original_distribution_plot"), height = "100%")
            )
          ),
          card(
            style = "aspect-ratio: 5/ 3; overflow: hidden;",
            full_screen = TRUE,
            div(class = "p-2", DT::dataTableOutput(ns("oringin_data_form"), width = "100%"))
          )
        ),
        layout_column_wrap(
          width = 1 / 2,
          height = NULL,
          fill = FALSE,
          gap = "20px",
          card(
            style = "aspect-ratio: 5/ 3; overflow: hidden;",
            full_screen = TRUE,
            div(class = "p-2", plotly::plotlyOutput(ns("discretized_data_visualization"), height = "100%"))
          ),
          card(
            style = "aspect-ratio: 5/ 3; overflow: hidden;",
            full_screen = TRUE,
            div(class = "p-2", DT::dataTableOutput(ns("discretized_data_form_ui"), width = "100%"))
          )
        )
      )
    )
  )
}

#' quantitative_discretization Server Functions
#'
#' @noRd
mod_quantitative_discretization_server <- function(id, global_store){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Rmd 内部工具函数：保持同名/同逻辑（不要用 qualitative_analysis 的版本替换）----
    discretize_percent <- function(numeric_input, numeric_percent) {
      group_lst <- rep(NA, length(numeric_input)) %>% unlist()
      for (i in 1:(length(numeric_percent) - 1)) {
        start_percent <- numeric_percent[[i]]
        stop_percent <- numeric_percent[[i + 1]]
        interval_name <- paste0(paste0(start_percent * 100, "%"), "~", paste0(stop_percent * 100, "%"))
        location <- which(
          numeric_input >= stats::quantile(numeric_input, start_percent, na.rm = TRUE) &
            numeric_input <= stats::quantile(numeric_input, stop_percent, na.rm = TRUE)
        )
        group_lst[location] <- interval_name
      }
      group_lst
    }

    discretize_numeric <- function(numeric_input, numeric_cut_points) {
      group_lst <- rep(NA, length(numeric_input)) %>% unlist()
      for (i in 1:(length(numeric_cut_points) - 1)) {
        start_value <- numeric_cut_points[[i]]
        stop_value <- numeric_cut_points[[i + 1]]
        interval_name <- paste0(start_value, "~", stop_value)
        location <- which(numeric_input >= start_value & numeric_input <= stop_value)
        group_lst[location] <- interval_name
      }
      group_lst
    }

    # ---- 控件 choices 动态更新（静态 UI，避免 renderUI 幽灵高度）----
    observe({
      req(global_store[["reactive_data"]])
      df_all <- global_store[["reactive_data"]]
      cols <- colnames(df_all)

      selected <- if ("定量结果" %in% cols) "定量结果" else cols[[1]]
      updateSelectInput(session, "chosed_continuous_col", choices = cols, selected = selected)
    })

    # ---- 核心 reactive：df / cut_points_info（从 Rmd 迁移，修正明显 bug 但保持语义）----
    df <- reactive({
      req(global_store[["filtered_data"]])
      req(input$chosed_continuous_col)
      validate(need(input$chosed_continuous_col %in% colnames(global_store[["filtered_data"]]), message = ""))
      df <- data.frame(original_value = global_store[["filtered_data"]][[input$chosed_continuous_col]])
      if (nrow(df) > 0) {
        df$trasformed_value <- df$original_value
      }
      df
    })

    cut_points_info <- reactive({
      req(df())

      if (!is.numeric(df()$original_value)) {
        text_group_names <- paste0(input$chosed_continuous_col, "不是数值变量")
        return(list(
          cut_points = "NA",
          df = "NA",
          group_levels = "NA",
          text_group_names = text_group_names,
          pattern = "NA"
        ))
      }

      df_ <- df()
      if (is.na(input$cut_points)) {
        cut_points <- c()
      } else {
        cut_points <- input$cut_points %>%
          strsplit(split = "|", fixed = TRUE) %>%
          unlist() %>%
          trimws() %>%
          unique()
      }

      # 结构化输入
      if (length(cut_points) == 0) {
        pattern <- "NA"
      } else if (all(grepl("%$", cut_points))) {
        numeric_percent <- gsub("%", "", cut_points) %>% as.numeric()
        numeric_percent <- numeric_percent / 100
        if (!any(is.na(numeric_percent))) {
          pattern <- "%"
        } else {
          pattern <- "NA"
        }
      } else {
        numeric_cut_points <- cut_points %>% as.numeric()
        if (!any(is.na(numeric_cut_points))) {
          pattern <- "numeric"
        } else {
          pattern <- "NA"
        }
      }

      if (pattern == "%") {
        cut_points <- c("0%", cut_points, "100%")
        numeric_percent <- c(0, numeric_percent, 1)[which(c(0, numeric_percent, 1) <= 1 & c(0, numeric_percent, 1) >= 0)] %>%
          sort() %>%
          unique()
        df_$discreted_group <- discretize_percent(df_$original_value, numeric_percent)
        df_ <- df_ %>% dplyr::arrange(original_value)
        group_levels <- unique(df_$discreted_group)
        text_group_names <- paste(unique(df_$discreted_group), collapse = ", ")
        text_group_names <- paste0("按", input$chosed_continuous_col, "分位数分组：", text_group_names)
      } else if (pattern == "numeric") {
        numeric_cut_points <- c(min(df_$original_value, na.rm = TRUE), numeric_cut_points, max(df_$original_value, na.rm = TRUE)) %>%
          unique() %>%
          sort()
        df_$discreted_group <- discretize_numeric(df_$original_value, numeric_cut_points)
        df_ <- df_ %>% dplyr::arrange(original_value)
        group_levels <- unique(df_$discreted_group)
        text_group_names <- paste(unique(df_$discreted_group), collapse = ", ")
        text_group_names <- paste0("按", input$chosed_continuous_col, "数值分组：", text_group_names)
      } else {
        text_group_names <- "请输入正确的分段信息，以“|”分割区间段，支持数值或百分位数"
        return(list(
          cut_points = "NA",
          df = "NA",
          group_levels = "NA",
          text_group_names = text_group_names,
          pattern = "NA"
        ))
      }

      list(
        cut_points = cut_points,
        df = df_,
        group_levels = group_levels,
        text_group_names = text_group_names,
        pattern = pattern
      )
    })

    output$text_group_names <- renderText({
      req(cut_points_info())
      cut_points_info()[["text_group_names"]]
    })

    observeEvent(input$cut_continuous_data, {
      # 更新分段后数据（按 Rmd：覆盖数据总线）
      validate(need(
        cut_points_info()[["text_group_names"]] != paste0(input$chosed_continuous_col, "不是数值变量"),
        message = ""
      ))
      if (cut_points_info()[["text_group_names"]] == paste0(input$chosed_continuous_col, "不是数值变量")) return()

      req(global_store[["filtered_data"]])
      data <- global_store[["filtered_data"]][, which(colnames(global_store[["filtered_data"]]) != paste0(input$chosed_continuous_col, "_离散"))]

      table1 <- cut_points_info()$df[, c("original_value", "discreted_group")] %>%
        dplyr::distinct()
      colnames(table1) <- c(input$chosed_continuous_col, paste0(input$chosed_continuous_col, "_离散"))

      col_order <- c(colnames(data), paste0(input$chosed_continuous_col, "_离散"))
      new_data <- merge(data, table1, by = input$chosed_continuous_col, all = TRUE)
      new_data <- new_data[, col_order]

      global_store[["reactive_data_na_"]] <- new_data
    })

    # ---- 原数据分布可视化（按 Rmd 的 Box-Cox/逆变换逻辑）----
    output$original_distribution_plot <- plotly::renderPlotly({
      req(df())
      req(input$select_transform)
      req(input$bin_num)
      validate(need(cut_points_info()[["pattern"]] != "NA", message = ""))

      df_plot <- df()
      df_plot$trasformed_value <- df_plot$original_value

      best_lambda <- NULL
      if (input$select_transform == "Box-Cox") {
        df_plot$trasformed_value <- as.numeric(df_plot$trasformed_value) + 0.1
        y_values <- df_plot$trasformed_value

        lambda_seq <- seq(-2, 2, by = 0.05)
        log_likelihoods <- sapply(lambda_seq, function(lambda) {
          y_trans <- if (lambda == 0) log(y_values) else (y_values^lambda - 1) / lambda

          n <- length(y_trans)
          var_y <- stats::var(y_trans)
          if (is.na(var_y) || var_y == 0) return(-Inf)

          -n / 2 * log(var_y) + (lambda - 1) * sum(log(y_values))
        })

        best_idx <- which.max(log_likelihoods)
        best_lambda <- lambda_seq[best_idx]

        df_plot$trasformed_value <- if (best_lambda == 0) {
          log(y_values)
        } else {
          ((y_values)^best_lambda - 1) / best_lambda
        }
      }

      breaks_transformed <- pretty(df_plot$trasformed_value, 8)
      breaks_original <- if (input$select_transform == "Box-Cox" && !is.null(best_lambda)) {
        round(boxcox_rev(breaks_transformed, best_lambda) - 0.01, 2)
      } else {
        breaks_transformed
      }

      quantile_5 <- stats::quantile(df_plot$trasformed_value, probs = 0.05, na.rm = TRUE)
      quantile_33 <- stats::quantile(df_plot$trasformed_value, probs = 0.33, na.rm = TRUE)
      quantile_66 <- stats::quantile(df_plot$trasformed_value, probs = 0.66, na.rm = TRUE)
      quantile_95 <- stats::quantile(df_plot$trasformed_value, probs = 0.95, na.rm = TRUE)

      get_color <- function(x) {
        purple <- "rgb(80, 0, 80)"
        if (x <= quantile_5) {
          return(purple)
        } else if (x <= quantile_33) {
          ratio <- (x - quantile_5) / (quantile_33 - quantile_5)
          r <- as.integer(80 - 80 * ratio)
          g <- 0
          b <- as.integer(80 + (139 - 80) * ratio)
          return(sprintf("rgb(%d, %d, %d)", r, g, b))
        } else if (x <= quantile_66) {
          ratio <- (x - quantile_33) / (quantile_66 - quantile_33)
          r <- 0
          g <- as.integer(100 * ratio)
          b <- as.integer(139 * (1 - ratio))
          return(sprintf("rgb(%d, %d, %d)", r, g, b))
        } else {
          ratio <- (x - quantile_66) / (max(df_plot$trasformed_value, na.rm = TRUE) - quantile_66)
          r <- as.integer(139 * ratio)
          g <- as.integer(100 * (1 - ratio))
          b <- 0
          return(sprintf("rgb(%d, %d, %d)", r, g, b))
        }
      }

      hist_data <- hist(df_plot$trasformed_value, plot = FALSE, breaks = input$bin_num)
      if (input$select_transform == "Box-Cox" && !is.null(best_lambda)) {
        hist_data$breaks_original <- round(boxcox_rev(hist_data$breaks, best_lambda) - 0.01, 2)
      } else {
        hist_data$breaks_original <- hist_data$breaks
      }

      bin_centers <- (hist_data$breaks[-1] + hist_data$breaks[-length(hist_data$breaks)]) / 2
      bar_colors <- sapply(bin_centers, get_color)

      cdf_values <- stats::ecdf(df_plot$trasformed_value)(hist_data$breaks[-1])
      cdf_counts <- cdf_values * length(df_plot$trasformed_value)

      hover_text <- sprintf(
        "数值范围: %.2f-%.2f<br>频数: %d<br>小于等于该值的样本数: %.0f (%.1f%%)",
        hist_data$breaks_original[-length(hist_data$breaks_original)],
        hist_data$breaks_original[-1],
        hist_data$counts,
        cdf_counts,
        cdf_values * 100
      )

      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          x = hist_data$mids,
          y = hist_data$counts,
          marker = list(color = bar_colors, line = list(color = "rgba(0,0,0,0.3)", width = 1)),
          name = "频数分布",
          hoverinfo = "text",
          hovertext = hover_text
        ) %>%
        plotly::layout(
          title = list(text = paste0(input$chosed_continuous_col, "原数据频数分布直方图"), x = 0.5, font = list(size = 16, color = "black")),
          xaxis = list(title = "数值大小", gridcolor = "lightgray", zerolinecolor = "lightgray", tickvals = breaks_transformed, ticktext = breaks_original),
          yaxis = list(title = paste0(input$chosed_continuous_col, "原数据频数"), gridcolor = "lightgray", zerolinecolor = "lightgray"),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          showlegend = FALSE
        )

      quantile_lines <- list(
        list(value = quantile_5, color = "purple", label = "5%"),
        list(value = quantile_33, color = "blue", label = "33%"),
        list(value = quantile_66, color = "green", label = "66%"),
        list(value = quantile_95, color = "red", label = "95%")
      )

      for (q_line in quantile_lines) {
        p <- p %>%
          plotly::add_segments(
            x = q_line$value, xend = q_line$value,
            y = 0, yend = max(hist_data$counts) * 0.95,
            line = list(color = q_line$color, dash = "dot", width = 1.5),
            hoverinfo = "none",
            name = paste0(q_line$label, "分位数")
          ) %>%
          plotly::add_annotations(
            x = q_line$value,
            y = max(hist_data$counts) * 0.95,
            text = q_line$label,
            showarrow = FALSE,
            bgcolor = "rgba(255,255,255,0.9)",
            font = list(color = q_line$color, size = 10),
            hoverinfo = "none",
            xanchor = "center"
          )
      }

      p
    })

    output$oringin_data_form <- DT::renderDataTable({
      validate(need(cut_points_info()[["pattern"]] != "NA", message = ""))
      req(df())

      quantile_lst <- lapply(seq(0, 1, 0.05), function(i) stats::quantile(df()$original_value, i, na.rm = TRUE))
      original_distribution_table <- data.frame(
        "分位数" = names(unlist(quantile_lst)),
        "数值" = unname(unlist(quantile_lst)),
        "小于等于该值的样本数" = unlist(lapply(unname(unlist(quantile_lst)), function(i) length(which(df()$original_value <= i))))
      )

      DT::datatable(
        original_distribution_table,
        extensions = c("Buttons"),
        options = list(
          pageLength = 40,
          dom = "lfrtip",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          autoWidth = FALSE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/zh.json")
        ),
        rownames = FALSE,
        width = "100%",
        style = "bootstrap"
      )
    })

    output$discretized_data_visualization <- plotly::renderPlotly({
      validate(need(cut_points_info()[["pattern"]] != "NA", message = ""))
      pattern <- cut_points_info()[["pattern"]]
      df2 <- cut_points_info()[["df"]]

      group_counts <- as.data.frame(table(df2$discreted_group), stringsAsFactors = FALSE)
      colnames(group_counts) <- c("Group", "Count")
      group_counts$Group <- factor(group_counts$Group, levels = cut_points_info()[["group_levels"]])

      hover_text_group <- sprintf(
        "分组: %s<br>频数: %d<br>占比: %.1f%%",
        group_counts$Group,
        group_counts$Count,
        group_counts$Count / nrow(df2) * 100
      )

      p_group <- plotly::plot_ly(
        data = group_counts,
        x = ~Group,
        y = ~Count,
        type = "bar",
        marker = list(color = "rgb(112, 112, 112)", line = list(color = "rgba(0,0,0,0.3)", width = 1)),
        hoverinfo = "text",
        hovertext = hover_text_group
      ) %>%
        plotly::layout(
          title = list(text = paste0(input$chosed_continuous_col, "分组分布直方图"), x = 0.5, font = list(size = 16, color = "black")),
          xaxis = list(title = "分组", gridcolor = "lightgray", zerolinecolor = "lightgray", tickangle = -45),
          yaxis = list(title = paste0(input$chosed_continuous_col, "各分组频数"), gridcolor = "lightgray", zerolinecolor = "lightgray"),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          showlegend = FALSE,
          margin = list(b = 100)
        ) %>%
        plotly::add_annotations(
          x = 0.02, y = 0.98, xref = "paper", yref = "paper",
          text = sprintf(
            "分组方式: %s<br>分组数量: %d<br>总样本数: %d",
            ifelse(pattern == "%", "百分比分组", ifelse(pattern == "numeric", "数值切点分组", "未分组")),
            nrow(group_counts),
            nrow(df2)
          ),
          showarrow = FALSE,
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "black",
          borderwidth = 1,
          font = list(size = 10),
          align = "left",
          xanchor = "left",
          yanchor = "top"
        )

      p_group
    })

    output$discretized_data_form_ui <- DT::renderDataTable({
      validate(need(cut_points_info()[["pattern"]] != "NA", message = ""))
      pattern <- cut_points_info()[["pattern"]]
      df2 <- cut_points_info()[["df"]]

      if (pattern == "%") {
        groups_distribution_table <- df2 %>%
          dplyr::group_by(discreted_group) %>%
          dplyr::reframe(
            Count = dplyr::n(),
            lower = round(min(original_value), 2),
            upper = round(max(original_value), 2)
          ) %>%
          dplyr::rename(组名 = discreted_group, 样本量 = Count, 定量下界 = lower, 定量上界 = upper) %>%
          dplyr::arrange(定量下界)
      } else {
        groups_distribution_table <- df2 %>%
          dplyr::group_by(discreted_group) %>%
          dplyr::reframe(
            Count = dplyr::n(),
            lower = get_quantile_position(df2$original_value, min(original_value)),
            upper = get_quantile_position(df2$original_value, max(original_value))
          ) %>%
          dplyr::rename(组名 = discreted_group, 样本量 = Count, 分位数下界 = lower, 分位数上界 = upper) %>%
          dplyr::arrange(分位数下界) %>%
          dplyr::mutate(
            分位数下界 = paste0(round(分位数下界, 4) * 100, "%"),
            分位数上界 = paste0(round(分位数上界, 4) * 100, "%")
          )

        if (nrow(groups_distribution_table) > 0) {
          groups_distribution_table[1, "分位数下界"] <- "0.00%"
          groups_distribution_table[nrow(groups_distribution_table), "分位数上界"] <- "100.00%"
        }
      }

      DT::datatable(
        groups_distribution_table,
        extensions = c("Buttons"),
        options = list(
          pageLength = 40,
          dom = "lfrtip",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          autoWidth = FALSE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/zh.json")
        ),
        rownames = FALSE,
        width = "100%",
        style = "bootstrap"
      )
    })
  })
}

## To be copied in the UI
# mod_quantitative_discretization_ui("quantitative_discretization_1")

## To be copied in the server
# mod_quantitative_discretization_server("quantitative_discretization_1")
