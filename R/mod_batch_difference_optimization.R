#' batch_difference_optimization UI Function
#'
#' @description A shiny Module for batch difference analysis with optimized statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput numericInput actionButton textOutput renderText tagAppendAttributes
#' @importFrom bslib nav_panel layout_sidebar sidebar navset_card_tab card card_header card_body
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_trace layout subplot
#' @importFrom dplyr filter arrange select group_by summarise mutate bind_rows left_join reframe n case_when
#' @importFrom rlang sym
#' @importFrom stats lm fitted residuals predict as.formula complete.cases quantile pbeta p.adjust IQR sd median
mod_batch_difference_optimization_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "批间差异分析",
    fillable = TRUE,
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "分析参数",
        width = "360px",
        bg = "#f8f9fa",
        open = "desktop",
        textInput(
          ns("regression_formula"),
          label = "回归公式自变量",
          value = "性别:I(年龄^2) + 性别:年龄 + 类别_无监督",
          width = "100%",
          placeholder = "例如: 性别:I(年龄^2) + 性别:年龄 + 类别_无监督"
        ),
        textOutput(ns("formula_validation")) %>% tagAppendAttributes(style = "color: #4DAF4A; font-weight: 500; font-size: 0.9rem; margin-top: 10px;"),
        numericInput(
          ns("n_value"),
          label = "n",
          value = 100,
          min = 10,
          max = 1000,
          step = 10,
          width = "100%"
        ),
        numericInput(
          ns("step_value"),
          label = "step",
          value = 100,
          min = 10,
          max = 1000,
          step = 10,
          width = "100%"
        ),
        div(class = "pt-3", actionButton(ns("start_calculation"), "开始计算", width = "100%", class = "btn-primary"))
      ),
      navset_card_tab(
        nav_panel(
          title = "折线图",
          card(
            full_screen = TRUE,
            card_header("多厂家多分位数等效水平分布"),
            card_body(
              padding = 0,
              plotly::plotlyOutput(ns("line_plot"), height = "100%", width = "100%")
            )
          )
        ),
        nav_panel(
          title = "小提琴图",
          card(
            full_screen = TRUE,
            card_header("多厂家各分位数等效波动分布"),
            card_body(
              padding = 0,
              plotly::plotlyOutput(ns("violin_plot"), height = "100%", width = "100%")
            )
          )
        )
      )
    )
  )
}

#' batch_difference_optimization Server Functions
#'
#' @noRd
mod_batch_difference_optimization_server <- function(id, global_store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 存储计算结果
    calculation_results <- reactiveVal(NULL)

    # 公式验证
    output$formula_validation <- renderText({
      req(input$regression_formula)
      formula_text <- trimws(input$regression_formula)
      
      if (formula_text == "") {
        return("⚠️ 请输入回归公式")
      }
      
      # 检查是否包含基本结构
      if (!grepl("\\+", formula_text) && !grepl("\\*", formula_text) && !grepl(":", formula_text)) {
        return("⚠️ 公式应包含交互项或加法项")
      }
      
      # 检查是否包含定量结果_transformed（不应该包含，因为这是因变量）
      if (grepl("定量结果", formula_text)) {
        return("⚠️ 公式中不应包含因变量'定量结果'")
      }
      
      # 尝试解析公式中的变量
      tryCatch({
        # 提取变量名（简单方法：查找单词边界）
        # 移除I()、:、+、*等符号，提取变量名
        vars <- gsub("I\\([^)]+\\)", "", formula_text)
        vars <- gsub("[:+*()^0-9.\\s]+", " ", vars)
        vars <- trimws(strsplit(vars, "\\s+")[[1]])
        vars <- vars[vars != ""]
        
        if (length(vars) == 0) {
          return("⚠️ 无法识别公式中的变量")
        }
        
        # 检查变量是否在数据中存在
        if (!is.null(global_store[["filtered_data"]])) {
          data_cols <- colnames(global_store[["filtered_data"]])
          missing_vars <- setdiff(vars, data_cols)
          if (length(missing_vars) > 0) {
            return(paste0("⚠️ 以下变量不在数据中: ", paste(missing_vars, collapse = ", ")))
          }
        }
        
        return("✅ 公式格式正确")
      }, error = function(e) {
        return(paste0("⚠️ 公式解析错误: ", e$message))
      })
    })

    # 提取公式中的变量名
    extract_formula_vars <- function(formula_text) {
      # 移除I()、数学符号等，提取变量名
      # 先处理I()表达式，提取其中的变量
      i_vars <- regmatches(formula_text, gregexpr("I\\(([^)]+)\\)", formula_text))[[1]]
      i_vars <- gsub("I\\(|\\)", "", i_vars)
      i_vars <- gsub("\\^[0-9]+", "", i_vars)  # 移除幂次
      i_vars <- trimws(i_vars)
      
      # 移除I()表达式
      vars <- gsub("I\\([^)]+\\)", "", formula_text)
      # 移除数学符号和数字
      vars <- gsub("[:+*()^0-9.\\s]+", " ", vars)
      vars <- trimws(strsplit(vars, "\\s+")[[1]])
      vars <- vars[vars != ""]
      
      # 合并I()中的变量和其他变量
      all_vars <- unique(c(i_vars, vars))
      all_vars <- all_vars[all_vars != ""]
      all_vars
    }

    # 获取基线水平
    return_base_line <- function(data, var, reagent_col = "试剂厂家") {
      if (!var %in% colnames(data) || !reagent_col %in% colnames(data)) {
        return(NULL)
      }
      
      count_ <- data %>%
        dplyr::group_by(!!rlang::sym(var), !!rlang::sym(reagent_col)) %>%
        dplyr::reframe(n = dplyr::n())
      
      if (nrow(count_) == 0) return(NULL)
      
      count_2 <- count_ %>%
        dplyr::group_by(!!rlang::sym(var)) %>%
        dplyr::reframe(n2 = dplyr::n(), count = min(n))
      
      unique_reagents <- length(unique(data[[reagent_col]]))
      base_line <- (count_2 %>%
        dplyr::filter(n2 == unique_reagents) %>%
        dplyr::filter(count == max(count)))[1, 1] %>%
        unlist()
      
      if (is.null(base_line) || length(base_line) == 0) {
        # 如果没有找到，返回最常见的值
        base_line <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
      }
      
      unname(base_line)
    }

    # 构建基线数据框
    build_base_line_df <- function(data, formula_vars) {
      # 初始化一个列表来存储基线值
      base_line_list <- list()
      
      # 年龄使用中位数
      if ("年龄" %in% colnames(data) && is.numeric(data[["年龄"]])) {
        median_age <- stats::median(data[["年龄"]], na.rm = TRUE)
        if (!is.na(median_age)) {
          base_line_list[["年龄"]] <- median_age
        }
      }
      
      # 其他变量使用return_base_line
      for (var in formula_vars) {
        if (var == "年龄") next  # 年龄已经处理
        if (var %in% colnames(data)) {
          base_line <- return_base_line(data, var)
          if (!is.null(base_line) && length(base_line) > 0 && !is.na(base_line)) {
            # 确保base_line是标量
            base_line_val <- if (length(base_line) == 1) base_line else base_line[1]
            base_line_list[[var]] <- base_line_val
          } else {
            # 如果return_base_line失败，使用最常见的值或中位数
            if (is.character(data[[var]]) || is.factor(data[[var]])) {
              freq_table <- table(data[[var]], useNA = "no")
              if (length(freq_table) > 0) {
                base_line_list[[var]] <- names(sort(freq_table, decreasing = TRUE))[1]
              }
            } else if (is.numeric(data[[var]])) {
              median_val <- stats::median(data[[var]], na.rm = TRUE)
              if (!is.na(median_val)) {
                base_line_list[[var]] <- median_val
              }
            }
          }
        }
      }
      
      # 如果列表为空，尝试用简单方法构建
      if (length(base_line_list) == 0) {
        for (var in formula_vars) {
          if (var %in% colnames(data)) {
            if (is.numeric(data[[var]])) {
              median_val <- stats::median(data[[var]], na.rm = TRUE)
              if (!is.na(median_val)) {
                base_line_list[[var]] <- median_val
              }
            } else {
              freq_table <- table(data[[var]], useNA = "no")
              if (length(freq_table) > 0) {
                base_line_list[[var]] <- names(sort(freq_table, decreasing = TRUE))[1]
              }
            }
          }
        }
      }
      
      # 将列表转换为数据框
      if (length(base_line_list) > 0) {
        base_line_df <- data.frame(base_line_list, stringsAsFactors = FALSE)
      } else {
        base_line_df <- data.frame(stringsAsFactors = FALSE)
      }
      
      base_line_df
    }

    # Box-Cox逆变换
    boxcox_rev <- function(y, lambda) {
      if (lambda == 0) {
        exp(y)
      } else {
        (lambda * y + 1)^(1 / lambda)
      }
    }

    # 核心计算逻辑
    observeEvent(input$start_calculation, {
      tryCatch({
        req(global_store[["filtered_data"]])
        req(input$regression_formula)
        req(input$n_value)
        req(input$step_value)

        data <- global_store[["filtered_data"]]
        validate(need(nrow(data) > 0, "请先上传数据"))
        validate(need("定量结果" %in% colnames(data), "数据中缺少'定量结果'列"))
        validate(need("采样时间" %in% colnames(data), "数据中缺少'采样时间'列"))

        showNotification("开始计算，请稍候...", type = "message")

        withProgress(message = "正在计算", value = 0, {
        # 数据预处理
        incProgress(0.1, detail = "数据预处理")
        data$定量结果 <- as.numeric(data$定量结果)
        data <- data %>% dplyr::filter(!is.na(定量结果))

        # 检查是否有试剂厂家列，如果没有则创建
        # 根据Rmd逻辑：data$试剂厂家 <- paste0(data$医院名称, data$试剂厂家)
        # 但这里如果已经有试剂厂家列，就不需要再创建
        if (!"试剂厂家" %in% colnames(data)) {
          # 如果医院名称和试剂厂家都存在，组合它们
          if ("医院名称" %in% colnames(data)) {
            # 检查是否有单独的试剂厂家列（可能名称不同）
            possible_manu_cols <- c("厂家", "制造商", "生产厂家", "试剂厂家")
            manu_col <- intersect(possible_manu_cols, colnames(data))
            if (length(manu_col) > 0) {
              data$试剂厂家 <- paste0(data$医院名称, data[[manu_col[1]]])
            } else {
              # 如果只有医院名称，使用医院名称作为厂家
              data$试剂厂家 <- data$医院名称
            }
          } else {
            # 如果都没有，创建默认厂家
            data$试剂厂家 <- "默认厂家"
          }
        } else if ("医院名称" %in% colnames(data)) {
          # 如果试剂厂家列已存在，但想按Rmd逻辑组合，可以这样做：
          # 但为了保持兼容性，如果已经有试剂厂家列，就不修改
          # data$试剂厂家 <- paste0(data$医院名称, data$试剂厂家)
        }

        # 剔除极端离群值
        find_outlier_indices <- function(x, para) {
          Q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
          Q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - para * IQR
          upper_bound <- Q3 + para * IQR
          which(x < lower_bound | x > upper_bound)
        }
        outlier_indices <- find_outlier_indices(data$定量结果, 100)
        if (length(outlier_indices) > 0) {
          data <- data[-outlier_indices, ]
        }

        # 时间变量处理
        data <- data %>% dplyr::arrange(采样时间)
        if (inherits(data$采样时间, c("POSIXt", "Date"))) {
          data$采样时间 <- as.Date(data$采样时间)
        }

        # Box-Cox转换
        incProgress(0.1, detail = "Box-Cox转换")
        data$定量结果_transformed <- as.numeric(data$定量结果) + 0.01
        y_values <- data$定量结果_transformed
        lambda_seq <- seq(-2, 2, by = 0.05)
        log_likelihoods <- sapply(lambda_seq, function(lambda) {
          if (lambda == 0) {
            y_trans <- log(y_values)
          } else {
            y_trans <- (y_values^lambda - 1) / lambda
          }
          n <- length(y_trans)
          var_y <- stats::var(y_trans)
          if (is.na(var_y)) return(-Inf)
          if (var_y == 0) return(-Inf)
          -n / 2 * log(var_y) + (lambda - 1) * sum(log(y_values))
        })
        best_idx <- which.max(log_likelihoods)
        best_lambda <- lambda_seq[best_idx]
        data$定量结果_transformed <- if (best_lambda == 0) {
          log(y_values)
        } else {
          ((y_values)^best_lambda - 1) / best_lambda
        }

        # 提取公式变量并构建基线数据框
        incProgress(0.1, detail = "构建回归模型")
        formula_vars <- extract_formula_vars(input$regression_formula)
        
        # 验证提取的变量
        if (length(formula_vars) == 0) {
          showNotification("无法从公式中提取变量，请检查公式格式", type = "error")
          return()
        }
        
        base_line_df <- build_base_line_df(data, formula_vars)
        
        # 验证base_line_df
        if (nrow(base_line_df) == 0 || ncol(base_line_df) == 0) {
          showNotification("无法构建基线数据框，请检查数据中的变量", type = "error")
          return()
        }
        
        # 确保base_line_df包含所有公式变量
        missing_vars_in_base <- setdiff(formula_vars, colnames(base_line_df))
        if (length(missing_vars_in_base) > 0) {
          showNotification(paste0("基线数据框缺少变量: ", paste(missing_vars_in_base, collapse = ", ")), type = "error")
          return()
        }

        # 构建回归公式
        formula_str <- paste0("定量结果_transformed ~ ", input$regression_formula)
        regression_formula <- tryCatch({
          stats::as.formula(formula_str)
        }, error = function(e) {
          showNotification(paste0("公式解析失败: ", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(regression_formula)) {
          return()
        }
        
        # 确保I()函数可用（stats包中的函数，用于在公式中保护表达式）
        # I()函数在公式中用于保护数学表达式，如I(年龄^2)

        # 按厂家计算
        manu_names_lst <- unique(data$试剂厂家)
        manu_result_lst <- list()
        quantile_lst <- c(0.1, 0.3, 0.5, 0.7, 0.9)

        total_manus <- length(manu_names_lst)
        for (manu_idx in seq_along(manu_names_lst)) {
          manu_name <- manu_names_lst[manu_idx]
          incProgress(0.6 / total_manus, detail = paste0("计算厂家: ", manu_name))

          # 筛选厂家数据
          # 确保所有需要的列都存在
          required_cols <- c("采样时间", "定量结果_transformed", "定量结果", formula_vars)
          available_cols <- intersect(required_cols, colnames(data))
          missing_cols <- setdiff(required_cols, available_cols)
          
          if (length(missing_cols) > 0) {
            showNotification(paste0("厂家 ", manu_name, " 缺少必要的列: ", paste(missing_cols, collapse = ", ")), type = "warning")
            next
          }
          
          # 确保base_line_df包含所有公式变量
          missing_base_vars <- setdiff(formula_vars, colnames(base_line_df))
          if (length(missing_base_vars) > 0) {
            showNotification(paste0("基线数据框缺少变量: ", paste(missing_base_vars, collapse = ", ")), type = "warning")
            # 尝试补充缺失的变量
            for (var in missing_base_vars) {
              if (var %in% colnames(data)) {
                if (is.numeric(data[[var]])) {
                  base_line_df[[var]] <- stats::median(data[[var]], na.rm = TRUE)
                } else {
                  freq_table <- table(data[[var]], useNA = "no")
                  if (length(freq_table) > 0) {
                    base_line_df[[var]] <- names(sort(freq_table, decreasing = TRUE))[1]
                  }
                }
              }
            }
          }
          
          data_subset <- data %>%
            dplyr::filter(试剂厂家 == manu_name) %>%
            dplyr::select(dplyr::all_of(available_cols)) %>%
            dplyr::filter(stats::complete.cases(.))
          
          # 确保数据不为空
          if (nrow(data_subset) == 0) {
            showNotification(paste0("厂家 ", manu_name, " 筛选后无有效数据"), type = "warning")
            next
          }

          if (nrow(data_subset) == 0) next

          # 拟合回归模型
          # 注意：I()函数在公式中用于保护数学表达式，如I(年龄^2)
          regression_model <- tryCatch({
            stats::lm(regression_formula, data = data_subset)
          }, error = function(e) {
            showNotification(paste0("厂家 ", manu_name, " 回归模型拟合失败: ", e$message), type = "warning")
            return(NULL)
          })
          
          if (is.null(regression_model)) next
          
          data_subset$预测值 <- stats::fitted(regression_model)
          data_subset$回归残差 <- stats::residuals(regression_model)
          base_line_prediction <- tryCatch({
            pred <- stats::predict(regression_model, newdata = base_line_df)
            # 确保pred是标量
            if (length(pred) > 0) {
              pred[1]
            } else {
              NULL
            }
          }, error = function(e) {
            showNotification(paste0("厂家 ", manu_name, " 基线预测失败: ", e$message), type = "warning")
            return(NULL)
          })
          
          if (is.null(base_line_prediction) || !is.finite(base_line_prediction)) {
            showNotification(paste0("厂家 ", manu_name, " 基线预测值为无效值"), type = "warning")
            next
          }

          # 计算窗口统计量
          win_cal_lst <- list()
          population <- data_subset$回归残差

          for (quantile_ in quantile_lst) {
            n <- input$n_value
            step <- input$step_value
            k <- round(n * quantile_, 0)

            beta_shape1 <- k
            beta_shape2 <- n - k + 1
            start_sample <- 1
            win_num <- (nrow(data_subset) - n) %/% step

            inner_results <- list()
            for (i in 0:win_num) {
              win_sample <- c(start_sample + i * step, start_sample + n + i * step)
              if (win_sample[2] > nrow(data_subset)) break
              
              testing_sample <- data_subset$回归残差[win_sample[1]:win_sample[2]]
              if (length(testing_sample) < k) next
              
              # 确保k不超过testing_sample的长度
              k_actual <- min(k, length(testing_sample))
              if (k_actual < 1) next

              X_k <- sort(testing_sample)[k_actual]
              if (length(population) == 0) next
              de_sample_population <- population[!(seq_along(population) %in% (win_sample[[1]]:win_sample[[2]]))]
              U_k <- (sum(de_sample_population <= X_k)) / length(de_sample_population)
              if (is.na(U_k) || !is.finite(U_k)) next
              prob_ <- stats::pbeta(U_k, beta_shape1, beta_shape2)
              if (is.na(prob_) || !is.finite(prob_)) next
              p_value <- min(prob_, 1 - prob_)

              result <- list(
                win_sample_start = win_sample[1],
                win_sample_stop = win_sample[2],
                start_time = data_subset$采样时间[win_sample[1]],
                stop_time = data_subset$采样时间[win_sample[2]],
                quantile_statistics = stats::quantile(testing_sample, quantile_),
                p_value = p_value
              )

              result$等效基线水平 <- ((stats::quantile(de_sample_population, quantile_) + base_line_prediction) %>%
                boxcox_rev(lambda = best_lambda)) - 0.01
              result$等效水平 <- ((base_line_prediction + result$quantile_statistics) %>%
                boxcox_rev(lambda = best_lambda)) - 0.01
              result$等效波动 <- result$等效水平 - result$等效基线水平

              inner_results[[length(inner_results) + 1]] <- result
            }

            if (length(inner_results) > 0) {
              results_df <- do.call(rbind, lapply(inner_results, function(x) {
                data.frame(
                  win_sample_start = x$win_sample_start,
                  win_sample_stop = x$win_sample_stop,
                  start_time = x$start_time,
                  stop_time = x$stop_time,
                  quantile_statistics = x$quantile_statistics,
                  p_value = x$p_value,
                  等效基线水平 = x$等效基线水平,
                  等效水平 = x$等效水平,
                  等效波动 = x$等效波动,
                  stringsAsFactors = FALSE
                )
              }))

              results_df$p_value_bonferroni <- stats::p.adjust(results_df$p_value, method = "BH") %>% round(7)
              win_cal_lst[[as.character(quantile_)]] <- results_df
            }
          }

          manu_result_lst[[manu_name]] <- win_cal_lst
        }

        # 整理所有数据
        incProgress(0.1, detail = "整理结果")
        all_manu_data <- data.frame()
        for (manu_name in names(manu_result_lst)) {
          current_win_cal_lst <- manu_result_lst[[manu_name]]
          for (i in seq_along(current_win_cal_lst)) {
            current_win_cal_lst[[i]]$quantile_level <- paste0(quantile_lst[[i]] * 100, "%")
          }
          names(current_win_cal_lst) <- paste0(quantile_lst * 100, "%")

          current_manu_data <- dplyr::bind_rows(current_win_cal_lst)
          if (nrow(current_manu_data) > 0) {
            current_manu_data$manu_name <- manu_name
            current_manu_data$mid_point <- (current_manu_data$win_sample_start + current_manu_data$win_sample_stop) / 2

            current_manu_data$hover_text <- paste(
              "厂家:", current_manu_data$manu_name, "<br>",
              "分位数:", current_manu_data$quantile_level, "<br>",
              "开始时间:", format(current_manu_data$start_time, "%Y-%m-%d"), "<br>",
              "结束时间:", format(current_manu_data$stop_time, "%Y-%m-%d"), "<br>",
              "窗口开始:", current_manu_data$win_sample_start, "<br>",
              "窗口结束:", current_manu_data$win_sample_stop, "<br>",
              "分位数统计量(变换后):", round(current_manu_data$quantile_statistics, 4), "<br>",
              "p-value:", round(current_manu_data$p_value_bonferroni, 4), "<br>",
              "等效水平:", round(current_manu_data$等效水平, 4), "<br>",
              "等效基线水平:", round(current_manu_data$等效基线水平, 4), "<br>",
              "等效波动:", round(current_manu_data$等效波动, 4), "<br>"
            )

            all_manu_data <- dplyr::bind_rows(all_manu_data, current_manu_data)
          }
        }

        # 计算统计摘要
        if (nrow(all_manu_data) > 0) {
          stats_summary <- all_manu_data %>%
            dplyr::group_by(manu_name, quantile_level) %>%
            dplyr::summarise(
              Q1 = stats::quantile(等效波动, 0.25, na.rm = TRUE),
              Q3 = stats::quantile(等效波动, 0.75, na.rm = TRUE),
              IQR_val = Q3 - Q1,
              Min = min(等效波动, na.rm = TRUE),
              Max = max(等效波动, na.rm = TRUE),
              Range_val = Max - Min,
              error_rate = length(which(p_value_bonferroni <= 0.05)) / dplyr::n(),
              .groups = "drop"
            )

          calculation_results(list(
            all_manu_data = all_manu_data,
            stats_summary = stats_summary,
            manu_result_lst = manu_result_lst,
            best_lambda = best_lambda
          ))

          showNotification("计算完成", type = "message")
        } else {
          showNotification("计算完成，但未生成有效结果", type = "warning")
        }
        })
      }, error = function(e) {
        showNotification(paste0("计算过程中发生错误: ", e$message), type = "error", duration = 10)
        print(paste0("错误详情: ", e$message))
        print(traceback())
      })
    })

    # 绘制折线图
    output$line_plot <- plotly::renderPlotly({
      req(calculation_results())
      results <- calculation_results()
      all_manu_data <- results$all_manu_data
      stats_summary <- results$stats_summary
      manu_result_lst <- results$manu_result_lst

      if (nrow(all_manu_data) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "暂无数据"))
      }

      p <- plotly::plot_ly()
      color_vec <- c("rgba(0, 0, 255, 0.5)", "rgba(0, 0, 255, 0.5)", "rgba(0, 0, 255, 0.5)")

      # 添加散点图
      for (manu_name in names(manu_result_lst)) {
        for (quantile_val in paste0(c(0.1, 0.3, 0.5, 0.7, 0.9) * 100, "%")) {
          current_data <- all_manu_data %>%
            dplyr::filter(manu_name == !!manu_name, quantile_level == !!quantile_val)

          if (nrow(current_data) == 0) next

          current_data <- current_data %>%
            dplyr::mutate(
              discrete_color = dplyr::case_when(
                p_value_bonferroni <= 0.01 ~ "red",
                p_value_bonferroni > 0.01 & p_value_bonferroni <= 0.05 ~ "orange",
                p_value_bonferroni > 0.05 ~ "green",
                TRUE ~ "gray"
              )
            )

          p <- p %>%
            plotly::add_trace(
              data = current_data,
              x = ~mid_point,
              y = ~等效水平,
              type = "scatter",
              mode = "markers",
              marker = list(
                size = 6,
                color = ~discrete_color,
                showscale = FALSE,
                opacity = 0.7
              ),
              hoverinfo = "text",
              text = ~hover_text,
              showlegend = FALSE,
              legendgroup = paste0(manu_name, "_", quantile_val)
            )
        }
      }

      # 添加折线图
      for (manu_name in names(manu_result_lst)) {
        for (quantile_val in paste0(c(0.1, 0.3, 0.5, 0.7, 0.9) * 100, "%")) {
          current_data <- all_manu_data %>%
            dplyr::filter(manu_name == !!manu_name, quantile_level == !!quantile_val)

          if (nrow(current_data) == 0) next

          current_stats <- stats_summary %>%
            dplyr::filter(manu_name == !!manu_name, quantile_level == !!quantile_val)

          legend_name <- if (nrow(current_stats) > 0) {
            paste0(
              manu_name, " - 分位数 ", quantile_val,
              " \n(IQR: ", round(current_stats$IQR_val, 2),
              ", Range: ", round(current_stats$Range_val, 2),
              ", Error Rate: ", round(current_stats$error_rate, 3), ")"
            )
          } else {
            paste0(manu_name, " - 分位数 ", quantile_val)
          }

          p <- p %>%
            plotly::add_trace(
              data = current_data,
              x = ~mid_point,
              y = ~等效水平,
              type = "scatter",
              mode = "lines",
              line = list(
                color = color_vec[1],
                width = 2
              ),
              hoverinfo = "none",
              name = legend_name,
              showlegend = TRUE,
              legendgroup = paste0(manu_name, "_", quantile_val),
              inherit = FALSE
            )
        }
      }

      # 获取项目名称（如果有）
      project_name <- if (!is.null(global_store[["filtered_data"]]) && "项目名称" %in% colnames(global_store[["filtered_data"]])) {
        unique(global_store[["filtered_data"]][["项目名称"]])[1]
      } else {
        "项目"
      }

      p %>%
        plotly::layout(
          title = paste0("多厂家", project_name, "多分位数等效水平分布"),
          xaxis = list(title = "窗口中心位置", showgrid = TRUE),
          yaxis = list(title = "等效水平", showgrid = TRUE),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top",
            font = list(size = 10),
            bgcolor = "rgba(255,255,255,0.8)"
          ),
          margin = list(l = 60, r = 60, t = 60, b = 120)
        )
    })

    # 绘制小提琴图
    output$violin_plot <- plotly::renderPlotly({
      req(calculation_results())
      results <- calculation_results()
      all_manu_data <- results$all_manu_data

      if (nrow(all_manu_data) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "暂无数据"))
      }

      quantile_levels <- unique(all_manu_data$quantile_level)
      all_sd <- stats::sd(all_manu_data$等效波动, na.rm = TRUE)
      silverman_bandwidth <- 1.3 * all_sd * nrow(all_manu_data)^(-1 / 5)

      violin_plots <- list()
      for (quantile_val in quantile_levels) {
        current_quantile_data <- all_manu_data %>%
          dplyr::filter(quantile_level == quantile_val)

        manu_stats <- current_quantile_data %>%
          dplyr::group_by(manu_name) %>%
          dplyr::summarise(
            iqr = stats::IQR(等效波动, na.rm = TRUE),
            range_val = diff(range(等效波动, na.rm = TRUE)),
            error_rate = length(which(p_value_bonferroni <= 0.05)) / dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            new_label = paste0(
              manu_name,
              "\nIQR:", round(iqr, 2),
              ", Range:", round(range_val, 2),
              "\nError Rate:", round(error_rate, 3)
            )
          )

        current_quantile_data <- current_quantile_data %>%
          dplyr::left_join(manu_stats %>% dplyr::select(manu_name, new_label), by = "manu_name")

        violin_plot <- plotly::plot_ly() %>%
          plotly::add_trace(
            data = current_quantile_data,
            x = ~manu_name,
            y = ~等效波动,
            type = "violin",
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "all",
            pointpos = -1.5,
            jitter = 0.1,
            scalemode = "width",
            bandwidth = silverman_bandwidth,
            hoverinfo = "text",
            text = ~hover_text,
            marker = list(size = 4, opacity = 0.7),
            color = ~manu_name,
            colors = "Set1",
            scalegroup = quantile_val
          ) %>%
          plotly::layout(
            title = paste0("", quantile_val, " - 各厂家等效波动"),
            xaxis = list(
              title = "",
              ticktext = manu_stats$new_label,
              tickvals = manu_stats$manu_name
            ),
            yaxis = list(title = paste0(quantile_val, "等效波动分布")),
            showlegend = FALSE,
            hoverlabel = list(bgcolor = "white", font = list(color = "black")),
            margin = list(b = 100)
          )

        violin_plots[[quantile_val]] <- violin_plot
      }

      # 合并小提琴图
      if (length(quantile_levels) <= 3) {
        combined_violin_plot <- plotly::subplot(
          violin_plots,
          nrows = 1,
          shareY = FALSE,
          titleX = TRUE,
          titleY = TRUE
        )
      } else {
        n_cols <- 2
        n_rows <- ceiling(length(quantile_levels) / n_cols)
        combined_violin_plot <- plotly::subplot(
          violin_plots,
          nrows = n_rows,
          shareY = FALSE,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.05
        )
      }

      project_name <- if (!is.null(global_store[["filtered_data"]]) && "项目名称" %in% colnames(global_store[["filtered_data"]])) {
        unique(global_store[["filtered_data"]][["项目名称"]])[1]
      } else {
        "项目"
      }

      combined_violin_plot %>%
        plotly::layout(
          title = paste0("多厂家", project_name, "各分位数等效波动分布"),
          showlegend = FALSE
        )
    })
  })
}

## To be copied in the UI
# mod_batch_difference_optimization_ui("batch_difference_optimization_1")

## To be copied in the server
# mod_batch_difference_optimization_server("batch_difference_optimization_1")
