#' batch_difference_optimization UI Function
#'
#' @description A shiny Module for batch difference analysis with optimized statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput numericInput actionButton textOutput renderText tagAppendAttributes modalDialog showModal modalButton observeEvent reactive reactiveVal HTML h4 tags renderUI htmlOutput
#' @importFrom bslib nav_panel layout_sidebar sidebar navset_card_tab card card_header card_body
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_trace layout subplot event_data
#' @importFrom dplyr filter arrange select group_by summarise mutate bind_rows left_join reframe n case_when
#' @importFrom rlang sym
#' @importFrom stats lm fitted residuals predict as.formula complete.cases quantile pbeta p.adjust IQR sd median density bw.nrd0 shapiro.test t.test wilcox.test chisq.test fisher.test
#' @importFrom DT renderDataTable datatable formatStyle
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
        # 保存原始行号，用于后续结果解读功能
        data$`_original_row_index` <- seq_len(nrow(data))
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
          required_cols <- c("采样时间", "定量结果_transformed", "定量结果", formula_vars, "_original_row_index")
          # 可选列：试剂盒批号和试剂盒盒号
          optional_cols <- c("试剂盒批号", "试剂盒盒号")
          available_cols <- intersect(required_cols, colnames(data))
          # 添加可选的批号-盒号列
          available_cols <- c(available_cols, intersect(optional_cols, colnames(data)))
          missing_cols <- setdiff(required_cols[-which(required_cols == "_original_row_index")], available_cols)
          
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

              # 记录窗口内数据对应的原始行号集合（用于结果解读功能）
              original_indices <- data_subset$`_original_row_index`[win_sample[1]:win_sample[2]]
              
              # 统计窗口内的批号和盒号信息
              batch_window_data <- data_subset[win_sample[1]:win_sample[2], , drop = FALSE]
              
              # 获取批号和盒号列（如果存在）
              试剂盒批号_col <- NULL
              试剂盒盒号_col <- NULL
              if ("试剂盒批号" %in% colnames(batch_window_data)) {
                试剂盒批号_col <- "试剂盒批号"
              }
              if ("试剂盒盒号" %in% colnames(batch_window_data)) {
                试剂盒盒号_col <- "试剂盒盒号"
              }
              
              # 统计批号-盒号组合的样本数量
              batch_lot_info <- ""
              if (!is.null(试剂盒批号_col) && !is.null(试剂盒盒号_col)) {
                # 处理批号和盒号，将NA、空值和字符串"NA"统一为"未知"
                batch_window_data$批号 <- sapply(batch_window_data[[试剂盒批号_col]], function(x) {
                  if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                    "未知"
                  } else {
                    as.character(x)
                  }
                })
                batch_window_data$盒号 <- sapply(batch_window_data[[试剂盒盒号_col]], function(x) {
                  if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                    "未知"
                  } else {
                    as.character(x)
                  }
                })
                
                # 统计每个批号-盒号组合的数量
                lot_box_counts <- batch_window_data %>%
                  dplyr::group_by(批号, 盒号) %>%
                  dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
                  dplyr::arrange(dplyr::desc(样本数))
                
                if (nrow(lot_box_counts) > 0) {
                  batch_lot_info <- paste0(
                    sapply(seq_len(nrow(lot_box_counts)), function(i) {
                      row <- lot_box_counts[i, ]
                      paste0("批号", row$批号, "-盒号", row$盒号, "：", row$样本数, "例")
                    }),
                    collapse = " <br>"
                  )
                }
              } else if (!is.null(试剂盒批号_col)) {
                # 只有批号列
                batch_window_data$批号 <- sapply(batch_window_data[[试剂盒批号_col]], function(x) {
                  if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                    "未知"
                  } else {
                    as.character(x)
                  }
                })
                
                lot_counts <- batch_window_data %>%
                  dplyr::group_by(批号) %>%
                  dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
                  dplyr::arrange(dplyr::desc(样本数))
                
                if (nrow(lot_counts) > 0) {
                  batch_lot_info <- paste0(
                    sapply(seq_len(nrow(lot_counts)), function(i) {
                      row <- lot_counts[i, ]
                      paste0("批号", row$批号, "：", row$样本数, "例")
                    }),
                    collapse = " <br>"
                  )
                }
              } else if (!is.null(试剂盒盒号_col)) {
                # 只有盒号列
                batch_window_data$盒号 <- sapply(batch_window_data[[试剂盒盒号_col]], function(x) {
                  if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                    "未知"
                  } else {
                    as.character(x)
                  }
                })
                
                box_counts <- batch_window_data %>%
                  dplyr::group_by(盒号) %>%
                  dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
                  dplyr::arrange(dplyr::desc(样本数))
                
                if (nrow(box_counts) > 0) {
                  batch_lot_info <- paste0(
                    sapply(seq_len(nrow(box_counts)), function(i) {
                      row <- box_counts[i, ]
                      paste0("盒号", row$盒号, "：", row$样本数, "例")
                    }),
                    collapse = " <br>"
                  )
                }
              }
              
              # 如果批号和盒号信息为空，标注为未知
              if (batch_lot_info == "" || is.null(batch_lot_info)) {
                batch_lot_info <- "批号-盒号信息：未知"
              }
              
              result <- list(
                win_sample_start = win_sample[1],
                win_sample_stop = win_sample[2],
                start_time = data_subset$采样时间[win_sample[1]],
                stop_time = data_subset$采样时间[win_sample[2]],
                quantile_statistics = stats::quantile(testing_sample, quantile_),
                p_value = p_value,
                original_indices = list(original_indices),  # 存储为列表以便后续转换为数据框
                batch_lot_info = batch_lot_info  # 存储批号-盒号信息
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
                  original_indices = I(list(x$original_indices[[1]])),  # 使用I()保留列表结构
                  batch_lot_info = x$batch_lot_info,  # 批号-盒号信息
                  stringsAsFactors = FALSE
                )
              }))

              results_df$p_value_bonferroni <- stats::p.adjust(results_df$p_value, method = "BH") %>% round(7)
              win_cal_lst[[as.character(quantile_)]] <- results_df
            }
          }

          # 保存该厂家的data_subset信息，用于后续重新计算
          manu_result_lst[[manu_name]] <- list(
            win_cal_lst = win_cal_lst,
            data_subset = data_subset,  # 保存完整的data_subset，包括回归残差和定量结果
            regression_model = regression_model,
            base_line_prediction = base_line_prediction
          )
        }

        # 整理所有数据
        incProgress(0.1, detail = "整理结果")
        all_manu_data <- data.frame()
        # 保存完整的manu_result_lst用于后续分析
        full_manu_result_lst <- manu_result_lst
        
        for (manu_name in names(manu_result_lst)) {
          current_result <- manu_result_lst[[manu_name]]
          current_win_cal_lst <- current_result$win_cal_lst
          for (i in seq_along(current_win_cal_lst)) {
            current_win_cal_lst[[i]]$quantile_level <- paste0(quantile_lst[[i]] * 100, "%")
          }
          names(current_win_cal_lst) <- paste0(quantile_lst * 100, "%")

          current_manu_data <- dplyr::bind_rows(current_win_cal_lst)
          if (nrow(current_manu_data) > 0) {
            current_manu_data$manu_name <- manu_name
            current_manu_data$mid_point <- (current_manu_data$win_sample_start + current_manu_data$win_sample_stop) / 2
            # 为每个点添加唯一标识符，用于点击事件识别
            current_manu_data$point_id <- paste0(
              manu_name, "_", 
              current_manu_data$quantile_level, "_", 
              seq_len(nrow(current_manu_data))
            )

            # 生成hover文本，包含批号-盒号信息
            hover_texts <- c()
            for (i in seq_len(nrow(current_manu_data))) {
              row <- current_manu_data[i, ]
              hover_text <- paste(
                "厂家:", row$manu_name, "<br>",
                "分位数:", row$quantile_level, "<br>",
                "开始时间:", format(row$start_time, "%Y-%m-%d"), "<br>",
                "结束时间:", format(row$stop_time, "%Y-%m-%d"), "<br>",
                "窗口开始:", row$win_sample_start, "<br>",
                "窗口结束:", row$win_sample_stop, "<br>",
                "分位数统计量(变换后):", round(row$quantile_statistics, 4), "<br>",
                "p-value:", round(row$p_value_bonferroni, 4), "<br>",
                "等效水平:", round(row$等效水平, 4), "<br>",
                "等效基线水平:", round(row$等效基线水平, 4), "<br>",
                "等效波动:", round(row$等效波动, 4), "<br>"
              )
              
              # 添加批号-盒号信息
              if (!is.null(row$batch_lot_info) && !is.na(row$batch_lot_info) && row$batch_lot_info != "") {
                hover_text <- paste0(hover_text, "试剂盒信息: <br>", row$batch_lot_info, "<br>")
              }
              
              hover_text <- paste0(hover_text, "点击查看原始数据")
              hover_texts <- c(hover_texts, hover_text)
            }
            
            current_manu_data$hover_text <- hover_texts

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

          # 创建point_id到原始索引的映射表，用于点击事件
          point_index_map <- list()
          for (i in seq_len(nrow(all_manu_data))) {
            point_id <- all_manu_data$point_id[i]
            original_indices <- all_manu_data$original_indices[[i]]
            point_index_map[[point_id]] <- original_indices
          }
          
          # 为每个窗口（batch）计算是否所有分位数都为绿色（p_value_bonferroni > 0.05）
          # 创建一个映射：窗口标识 -> 是否全绿
          window_all_green_map <- list()
          # 收集所有全绿batch的数据作为对照组
          all_green_batch_data <- list()
          
          for (manu_name in unique(all_manu_data$manu_name)) {
            manu_data <- all_manu_data %>% dplyr::filter(manu_name == !!manu_name)
            # 按窗口分组（使用win_sample_start和win_sample_stop），一个batch有5个分位数
            windows <- manu_data %>%
              dplyr::group_by(win_sample_start, win_sample_stop) %>%
              dplyr::summarise(
                all_green = all(p_value_bonferroni > 0.05),
                count = dplyr::n(),  # 应该是5个分位数
                .groups = "drop"
              ) %>%
              dplyr::filter(count == length(quantile_lst))  # 确保有完整的5个分位数
            
            for (i in seq_len(nrow(windows))) {
              window_key <- paste0(manu_name, "_", windows$win_sample_start[i], "_", windows$win_sample_stop[i])
              is_all_green <- windows$all_green[i]
              window_all_green_map[[window_key]] <- is_all_green
              
              # 如果是全绿batch，收集其数据
              if (is_all_green && manu_name %in% names(full_manu_result_lst)) {
                manu_result <- full_manu_result_lst[[manu_name]]
                data_subset <- manu_result$data_subset
                win_start <- windows$win_sample_start[i]
                win_stop <- windows$win_sample_stop[i]
                
                # 获取该batch的数据（使用回归残差和定量结果）
                batch_indices <- win_start:win_stop
                batch_data <- data_subset[batch_indices, , drop = FALSE]
                
                # 存储batch标识和数据
                all_green_batch_data[[window_key]] <- list(
                  manu_name = manu_name,
                  win_start = win_start,
                  win_stop = win_stop,
                  batch_indices = batch_indices,
                  data = batch_data
                )
              }
            }
          }
          
          # 合并所有全绿batch的数据作为对照组
          control_group_residuals <- c()
          control_group_original <- c()
          control_group_data <- list()
          
          if (length(all_green_batch_data) > 0) {
            for (batch_key in names(all_green_batch_data)) {
              batch_info <- all_green_batch_data[[batch_key]]
              batch_data <- batch_info$data
              
              # 收集回归残差和原数据
              if ("回归残差" %in% colnames(batch_data)) {
                control_group_residuals <- c(control_group_residuals, batch_data$回归残差)
              }
              if ("定量结果" %in% colnames(batch_data)) {
                control_group_original <- c(control_group_original, batch_data$定量结果)
              }
              
              # 收集完整的协变量数据（用于富集分析）
              formula_vars <- extract_formula_vars(input$regression_formula)
              for (var in formula_vars) {
                if (var %in% colnames(batch_data)) {
                  var_data <- batch_data[[var]]
                  # 移除NA值
                  var_data <- var_data[!is.na(var_data)]
                  
                  if (length(var_data) > 0) {
                    if (is.null(control_group_data[[var]])) {
                      control_group_data[[var]] <- c()
                    }
                    # 如果是因子，转换为字符以保持一致性
                    if (is.factor(var_data)) {
                      var_data <- as.character(var_data)
                    }
                    control_group_data[[var]] <- c(control_group_data[[var]], var_data)
                  }
                }
              }
            }
          }
          
          calculation_results(list(
            all_manu_data = all_manu_data,
            stats_summary = stats_summary,
            manu_result_lst = full_manu_result_lst,  # 保存完整信息
            best_lambda = best_lambda,
            point_index_map = point_index_map,
            window_all_green_map = window_all_green_map,
            all_green_batch_data = all_green_batch_data,  # 保存所有全绿batch的详细信息
            control_group_residuals = control_group_residuals,  # 对照组回归残差
            control_group_original = control_group_original,  # 对照组原数据
            control_group_covariates = control_group_data,  # 对照组协变量
            quantile_lst = quantile_lst,
            n_value = input$n_value,
            step_value = input$step_value,
            regression_formula = input$regression_formula
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

      p <- plotly::plot_ly(source = "line_plot_click")  # 设置source ID用于事件监听
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
              legendgroup = paste0(manu_name, "_", quantile_val),
              key = ~point_id,  # 添加key属性用于事件识别
              customdata = ~point_id  # 添加customdata用于JavaScript事件
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
              inherit = FALSE,
              key = ~point_id,  # 为折线也添加key属性，用于点击事件
              customdata = ~point_id
            )
        }
      }

      # 获取项目名称（如果有）
      project_name <- if (!is.null(global_store[["filtered_data"]]) && "项目名称" %in% colnames(global_store[["filtered_data"]])) {
        unique(global_store[["filtered_data"]][["项目名称"]])[1]
      } else {
        "项目"
      }

      p <- p %>%
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
      
      p
    })

    # 存储当前点击点对应的原始数据（用于模态窗口显示）
    clicked_original_data <- reactiveVal(NULL)
    clicked_point_info <- reactiveVal(NULL)
    clicked_window_info <- reactiveVal(NULL)  # 存储窗口信息，用于重新计算
    
    # 监听plotly点击事件，显示原始数据
    click_data <- reactive({
      plotly::event_data("plotly_click", source = "line_plot_click")
    })
    
    # 显示原始数据的模态窗口
    observeEvent(click_data(), {
      req(click_data())
      req(calculation_results())
      req(global_store[["filtered_data"]])
      
      # 获取点击的点信息
      click_point <- click_data()
      
      # 从key或customdata中获取point_id
      point_id <- NULL
      if (!is.null(click_point$key) && length(click_point$key) > 0) {
        point_id <- click_point$key[1]
      } else if (!is.null(click_point$customdata) && length(click_point$customdata) > 0) {
        point_id <- click_point$customdata[1]
      }
      
      if (is.null(point_id)) {
        # 如果没有key，尝试从all_manu_data中查找匹配的点
        results <- calculation_results()
        all_manu_data <- results$all_manu_data
        
        if (nrow(all_manu_data) > 0 && !is.null(click_point$pointNumber)) {
          # 通过pointNumber和曲线信息查找对应的点
          # 这里需要更复杂的逻辑来匹配点，暂时使用pointNumber
          if (click_point$pointNumber[1] < nrow(all_manu_data)) {
            point_id <- all_manu_data$point_id[click_point$pointNumber[1] + 1]
          }
        }
      }
      
      if (is.null(point_id)) {
        showNotification("无法识别点击的点，请重试", type = "warning")
        return()
      }
      
      # 从映射表中获取原始索引
      results <- calculation_results()
      point_index_map <- results$point_index_map
      
      if (is.null(point_index_map) || !point_id %in% names(point_index_map)) {
        showNotification("无法找到该点对应的原始数据", type = "warning")
        return()
      }
      
      original_indices <- point_index_map[[point_id]]
      
      if (is.null(original_indices) || length(original_indices) == 0) {
        showNotification("该点没有对应的原始数据", type = "warning")
        return()
      }
      
      # 从filtered_data中提取对应的原始数据
      filtered_data <- global_store[["filtered_data"]]
      
      # 确保索引在有效范围内
      valid_indices <- original_indices[original_indices >= 1 & original_indices <= nrow(filtered_data)]
      
      if (length(valid_indices) == 0) {
        showNotification("原始数据索引无效", type = "warning")
        return()
      }
      
      # 提取原始数据行
      original_data_rows <- filtered_data[valid_indices, , drop = FALSE]
      
      # 获取该点的详细信息
      all_manu_data <- results$all_manu_data
      point_info <- all_manu_data[all_manu_data$point_id == point_id, ]
      
      if (nrow(point_info) == 0) {
        showNotification("无法找到该点的详细信息", type = "warning")
        return()
      }
      
      # 检查该窗口（batch）是否所有分位数都为绿色（用于显示标签）
      results <- calculation_results()
      window_key <- paste0(
        point_info$manu_name[1], "_", 
        point_info$win_sample_start[1], "_", 
        point_info$win_sample_stop[1]
      )
      is_all_green <- if (!is.null(results$window_all_green_map) && window_key %in% names(results$window_all_green_map)) {
        results$window_all_green_map[[window_key]]
      } else {
        FALSE
      }
      
      # 存储原始数据和点信息，供表格渲染使用
      clicked_original_data(original_data_rows)
      clicked_point_info(point_info)
      
      # 存储窗口信息，用于重新计算testing_sample和de_sample_population
      window_info <- list(
        manu_name = point_info$manu_name[1],
        win_sample_start = point_info$win_sample_start[1],
        win_sample_stop = point_info$win_sample_stop[1],
        window_key = window_key,
        is_all_green = is_all_green
      )
      clicked_window_info(window_info)
      
      # 创建模态窗口显示原始数据
      # 添加自定义样式使宽度为xl的1.5倍（xl约为1140px，1.5倍约为1710px）
      showModal(modalDialog(
        tags$style(HTML("
          .modal-dialog.modal-xl {
            max-width: 1710px;
          }
        ")),
        title = tagList(
          h4("结果解读"),
          tags$div(
            style = "font-size: 0.9em; color: #666; margin-top: 5px;",
            HTML(paste0(
              "<strong>厂家:</strong> ", point_info$manu_name[1], " | ",
              "<strong>分位数:</strong> ", point_info$quantile_level[1], " | ",
              "<strong>窗口:</strong> ", point_info$win_sample_start[1], "-", point_info$win_sample_stop[1], " | ",
              "<strong>时间范围:</strong> ", format(point_info$start_time[1], "%Y-%m-%d"), 
              " 至 ", format(point_info$stop_time[1], "%Y-%m-%d"), " | ",
              "<strong>数据行数:</strong> ", nrow(original_data_rows),
              if (is_all_green) " | <span style='color: green;'><strong>全绿通过</strong></span>" else ""
            ))
          )
        ),
        size = "xl",  # 使用xl尺寸，但通过CSS覆盖宽度为1.5倍
        easyClose = TRUE,
        footer = modalButton("关闭"),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "详细解读",
            tags$div(
              style = "height: 70vh; overflow-y: auto;",
              # 核密度估计可视化和协变量富集分析（对所有点都显示）
              tagList(
                tags$h5("结果解读"),
                tags$div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                  htmlOutput(ns("interpretation_text"))
                ),
                tags$hr(),
                tags$h5("分布对比分析"),
                tags$p("对照组：所有全绿batch（五个分位数都通过检验）的数据集合"),
                plotly::plotlyOutput(ns("density_plot"), height = "400px"),
                tags$hr(),
                tags$h5("协变量富集分析"),
                tags$p("分析测试batch与全绿batch对照组在协变量分布上的差异"),
                DT::dataTableOutput(ns("enrichment_table"))
              )
            )
          ),
          bslib::nav_panel(
            title = "原始数据",
            tags$div(
              style = "height: 70vh;",  # 设置固定高度为视口高度的70%
              DT::dataTableOutput(ns("original_data_table"), height = "100%", fill = TRUE)
            )
          )
        )
      ))
    })
    
    # 渲染原始数据表格
    output$original_data_table <- DT::renderDataTable({
      req(clicked_original_data())
      original_data_rows <- clicked_original_data()
      
      # 计算隐藏列索引逻辑（与"当前数据"板块保持一致）
      target_hide_cols <- c("样本类型", "方法学", "项目序号", "结果单位", "参考区间", "备注信息", "调研人")
      actual_hide_cols <- intersect(names(original_data_rows), target_hide_cols)
      hidden_indices <- which(names(original_data_rows) %in% actual_hide_cols) - 1
      
      DT::datatable(
        original_data_rows,
        # 使用与"当前数据"板块相同的样式配置
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
          scrollX = TRUE,
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
          columnDefs = list(
            list(targets = hidden_indices, visible = FALSE)
          ),
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json',
            buttons = list(colvis = '列可见性')
          )
        )
      ) %>%
        DT::formatStyle(
          columns = names(original_data_rows),
          fontSize = '14px',
          lineHeight = '20px'
        )
    })
    
    # 生成自然语言结果解读
    output$interpretation_text <- renderUI({
      req(clicked_window_info())
      req(calculation_results())
      
      window_info <- clicked_window_info()
      results <- calculation_results()
      
      # 获取该batch的所有5个分位数的数据
      manu_name <- window_info$manu_name
      win_start <- window_info$win_sample_start
      win_stop <- window_info$win_sample_stop
      
      batch_data <- results$all_manu_data %>%
        dplyr::filter(
          manu_name == !!manu_name,
          win_sample_start == !!win_start,
          win_sample_stop == !!win_stop
        ) %>%
        dplyr::arrange(quantile_level)
      
      if (nrow(batch_data) == 0) {
        return(HTML("<p>无法获取该batch的分位数数据</p>"))
      }
      
      # 获取该batch的批号-盒号信息（从第一条记录获取，因为同一batch的所有分位数应该相同）
      batch_lot_info <- ""
      if ("batch_lot_info" %in% colnames(batch_data)) {
        batch_lot_info <- batch_data$batch_lot_info[1]
        if (is.na(batch_lot_info) || batch_lot_info == "") {
          batch_lot_info <- "批号-盒号信息：未知"
        }
      } else {
        # 如果数据中没有批号-盒号信息，尝试从原始数据中重新计算
        tryCatch({
          manu_result <- results$manu_result_lst[[manu_name]]
          data_subset <- manu_result$data_subset
          batch_window_data <- data_subset[win_start:win_stop, , drop = FALSE]
          
          试剂盒批号_col <- NULL
          试剂盒盒号_col <- NULL
          if ("试剂盒批号" %in% colnames(batch_window_data)) {
            试剂盒批号_col <- "试剂盒批号"
          }
          if ("试剂盒盒号" %in% colnames(batch_window_data)) {
            试剂盒盒号_col <- "试剂盒盒号"
          }
          
          if (!is.null(试剂盒批号_col) && !is.null(试剂盒盒号_col)) {
            batch_window_data$批号 <- sapply(batch_window_data[[试剂盒批号_col]], function(x) {
              if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                "未知"
              } else {
                as.character(x)
              }
            })
            batch_window_data$盒号 <- sapply(batch_window_data[[试剂盒盒号_col]], function(x) {
              if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                "未知"
              } else {
                as.character(x)
              }
            })
            
            lot_box_counts <- batch_window_data %>%
              dplyr::group_by(批号, 盒号) %>%
              dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
              dplyr::arrange(dplyr::desc(样本数))
            
            if (nrow(lot_box_counts) > 0) {
              batch_lot_info <- paste0(
                sapply(seq_len(nrow(lot_box_counts)), function(i) {
                  row <- lot_box_counts[i, ]
                  paste0("批号", row$批号, "-盒号", row$盒号, "：", row$样本数, "例")
                }),
                collapse = " <br>"
              )
            }
          } else if (!is.null(试剂盒批号_col)) {
            # 只有批号列
            batch_window_data$批号 <- sapply(batch_window_data[[试剂盒批号_col]], function(x) {
              if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                "未知"
              } else {
                as.character(x)
              }
            })
            
            lot_counts <- batch_window_data %>%
              dplyr::group_by(批号) %>%
              dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
              dplyr::arrange(dplyr::desc(样本数))
            
            if (nrow(lot_counts) > 0) {
              batch_lot_info <- paste0(
                sapply(seq_len(nrow(lot_counts)), function(i) {
                  row <- lot_counts[i, ]
                  paste0("批号", row$批号, "：", row$样本数, "例")
                }),
                collapse = " <br>"
              )
            }
          } else if (!is.null(试剂盒盒号_col)) {
            # 只有盒号列
            batch_window_data$盒号 <- sapply(batch_window_data[[试剂盒盒号_col]], function(x) {
              if (is.na(x) || x == "" || as.character(x) == "NA" || trimws(as.character(x)) == "") {
                "未知"
              } else {
                as.character(x)
              }
            })
            
            box_counts <- batch_window_data %>%
              dplyr::group_by(盒号) %>%
              dplyr::summarise(样本数 = dplyr::n(), .groups = "drop") %>%
              dplyr::arrange(dplyr::desc(样本数))
            
            if (nrow(box_counts) > 0) {
              batch_lot_info <- paste0(
                sapply(seq_len(nrow(box_counts)), function(i) {
                  row <- box_counts[i, ]
                  paste0("盒号", row$盒号, "：", row$样本数, "例")
                }),
                collapse = " <br>"
              )
            }
          }
        }, error = function(e) {
          batch_lot_info <- "批号-盒号信息：未知"
        })
      }
      
      # 分析每个分位数的表现
      quantile_descriptions <- list()
      quantile_patterns <- list()
      
      for (i in seq_len(nrow(batch_data))) {
        row <- batch_data[i, ]
        quantile_level <- row$quantile_level
        p_value <- row$p_value_bonferroni
        等效波动 <- row$等效波动
        
        # 判断表现类型
        if (p_value <= 0.05) {
          if (等效波动 > 0) {
            pattern <- "显著升高"
            quantile_patterns[[i]] <- "显著升高"
          } else {
            pattern <- "显著降低"
            quantile_patterns[[i]] <- "显著降低"
          }
        } else {
          if (abs(等效波动) < 0.01) {  # 接近0认为是正常
            pattern <- "正常"
            quantile_patterns[[i]] <- "正常"
          } else if (等效波动 > 0) {
            pattern <- "升高"
            quantile_patterns[[i]] <- "升高"
          } else {
            pattern <- "降低"
            quantile_patterns[[i]] <- "降低"
          }
        }
        
        quantile_descriptions[[i]] <- paste0(
          quantile_level, "分位数", pattern,
          "（等效波动：", round(等效波动, 4), 
          "，p值：", round(p_value, 4), "）"
        )
      }
      
      # 生成分位数描述文本
      quantile_text <- paste(quantile_descriptions, collapse="；")
      
      # 生成综合结论 - 以显著变化为主要依据
      # 统计各模式的数量和具体位置
      pattern_vector <- unlist(quantile_patterns)
      pattern_counts <- table(pattern_vector)
      n_significant_up <- sum(pattern_vector == "显著升高")
      n_significant_down <- sum(pattern_vector == "显著降低")
      n_up <- sum(pattern_vector == "升高")
      n_down <- sum(pattern_vector == "降低")
      n_normal <- sum(pattern_vector == "正常")
      
      # 识别显著变化的具体分位数位置
      significant_up_quantiles <- batch_data$quantile_level[pattern_vector == "显著升高"]
      significant_down_quantiles <- batch_data$quantile_level[pattern_vector == "显著降低"]
      up_quantiles <- batch_data$quantile_level[pattern_vector == "升高"]
      down_quantiles <- batch_data$quantile_level[pattern_vector == "降低"]
      normal_quantiles <- batch_data$quantile_level[pattern_vector == "正常"]
      
      # 判断整体趋势 - 以显著变化为主导，详细描述所有显著变化
      conclusion <- ""
      conclusion_details <- ""
      warning_msg <- ""
      manufacturer_msg <- ""
      
      # 首先描述所有显著变化的具体情况
      significant_details <- c()
      if (length(significant_up_quantiles) > 0) {
        significant_details <- c(significant_details, 
          paste0(significant_up_quantiles, collapse="、"), "分位数显著升高")
      }
      if (length(significant_down_quantiles) > 0) {
        significant_details <- c(significant_details,
          paste0(significant_down_quantiles, collapse="、"), "分位数显著降低")
      }
      
      # 情况1：所有分位数都显著升高/降低 -> 系统性偏倚
      if (n_significant_up == 5) {
        conclusion <- "该batch的所有分位数（10%、30%、50%、70%、90%）均显著升高，提示存在系统性正偏倚。"
        conclusion_details <- "所有分位数的一致性显著升高表明这是系统性的正偏倚，而非偶然波动。"
        warning_msg <- "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到系统性正偏倚，可能原因包括：试剂批号更换、校准偏移、仪器漂移或质控品异常。建议立即检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
        manufacturer_msg <- "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到严重的系统性正偏倚，提示该批次试剂可能存在质量问题。建议：1) 检查该批次的生产记录，重点关注校准品配制、抗体效价、反应体系pH值等关键参数；2) 对比该批次与正常批次的生产工艺差异；3) 评估是否需要召回该批次产品；4) 加强该批次的质量追溯和留样检测；5) 如为持续性问题，需审查生产工艺稳定性和原材料质量控制。"
      } else if (n_significant_down == 5) {
        conclusion <- "该batch的所有分位数（10%、30%、50%、70%、90%）均显著降低，提示存在系统性负偏倚。"
        conclusion_details <- "所有分位数的一致性显著降低表明这是系统性的负偏倚，而非偶然波动。"
        warning_msg <- "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到系统性负偏倚，可能原因包括：试剂批号更换、校准偏移、仪器漂移或质控品异常。建议立即检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
        manufacturer_msg <- "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到严重的系统性负偏倚，提示该批次试剂可能存在质量问题。建议：1) 检查该批次的生产记录，重点关注校准品配制、抗体效价、反应体系pH值等关键参数；2) 对比该批次与正常批次的生产工艺差异；3) 评估是否需要召回该批次产品；4) 加强该批次的质量追溯和留样检测；5) 如为持续性问题，需审查生产工艺稳定性和原材料质量控制。"
      }
      # 情况2：有显著升高和显著降低同时存在（反向变化）
      else if (n_significant_up > 0 && n_significant_down > 0) {
        # 检查显著变化的分位数位置关系
        low_quantiles <- c("10%", "30%")
        mid_quantiles <- "50%"
        high_quantiles <- c("70%", "90%")
        
        sig_low_up <- sum(significant_up_quantiles %in% low_quantiles)
        sig_low_down <- sum(significant_down_quantiles %in% low_quantiles)
        sig_mid_up <- sum(significant_up_quantiles == mid_quantiles)
        sig_mid_down <- sum(significant_down_quantiles == mid_quantiles)
        sig_high_up <- sum(significant_up_quantiles %in% high_quantiles)
        sig_high_down <- sum(significant_down_quantiles %in% high_quantiles)
        
        # 低分位数显著升高，高分位数显著降低 -> 分布压缩
        if (sig_low_up >= 1 && sig_high_down >= 1) {
          conclusion <- paste0("该batch呈现显著的浓度依赖型变化：", 
            paste0(significant_up_quantiles, collapse="、"), "分位数显著升高，",
            paste0(significant_down_quantiles, collapse="、"), "分位数显著降低，",
            "提示数据分布发生压缩或非线性响应。")
          conclusion_details <- "低分位数显著升高而高分位数显著降低，表明试剂盒在低浓度区间的检测值系统性偏高，在高浓度区间的检测值系统性偏低，可能提示线性范围问题或Hook效应。"
          warning_msg <- "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到显著的浓度依赖型偏差，低浓度样本可能偏高，高浓度样本可能偏低。建议检查仪器的线性验证结果和校准曲线，必要时进行线性范围验证。"
          manufacturer_msg <- "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到显著的非线性响应模式，提示试剂盒的线性范围或Hook效应存在问题。建议：1) 紧急检查试剂盒的线性验证数据，确认线性范围是否满足设计要求；2) 重点评估是否存在Hook效应（高浓度样本的假性降低）；3) 检查校准曲线的拟合质量和校准点设置是否合理；4) 评估是否需要调整校准品浓度范围或增加校准点；5) 如为系统性问题，需优化反应体系设计或抗体配对。"
        }
        # 低分位数显著降低，高分位数显著升高 -> 分布扩展
        else if (sig_low_down >= 1 && sig_high_up >= 1) {
          conclusion <- paste0("该batch呈现显著的浓度依赖型变化：", 
            paste0(significant_down_quantiles, collapse="、"), "分位数显著降低，",
            paste0(significant_up_quantiles, collapse="、"), "分位数显著升高，",
            "提示数据分布发生扩展或精密度变化。")
          conclusion_details <- "低分位数显著降低而高分位数显著升高，表明试剂盒在低浓度区间的检测值系统性偏低，在高浓度区间的检测值系统性偏高，可能提示精密度问题或反应体系不稳定。"
          warning_msg <- "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到显著的浓度依赖型偏差，低浓度样本可能偏低，高浓度样本可能偏高。建议检查质控CV值、批间精密度和试剂稳定性。"
          manufacturer_msg <- "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到显著的精密度变化模式，提示试剂盒的重复性或稳定性存在问题。建议：1) 立即检查该批次的内精密度（CV值）数据，对比历史批次；2) 评估试剂的稳定性，特别是开瓶后的稳定性；3) 检查生产过程中的环境控制（温度、湿度、洁净度）是否达标；4) 评估原材料的批间变异，特别是关键原材料（如抗体、酶等）；5) 如为持续性问题，需加强生产工艺的标准化和质量控制。"
        }
        # 其他显著变化组合（如中间显著升高，两端显著降低等）
        else {
          conclusion <- paste0("该batch存在显著的分布变化：", 
            if (length(significant_up_quantiles) > 0) paste0(significant_up_quantiles, collapse="、"), "分位数显著升高",
            if (length(significant_up_quantiles) > 0 && length(significant_down_quantiles) > 0) "，",
            if (length(significant_down_quantiles) > 0) paste0(significant_down_quantiles, collapse="、"), "分位数显著降低",
            "，提示在不同浓度区间存在不一致的系统性偏差。")
          conclusion_details <- paste0("显著的", if (length(significant_up_quantiles) > 0) "升高", 
            if (length(significant_up_quantiles) > 0 && length(significant_down_quantiles) > 0) "和",
            if (length(significant_down_quantiles) > 0) "降低", 
            "变化表明该批次试剂在不同浓度区间的表现存在显著差异，需要重点关注。")
          warning_msg <- "<strong style='color: #f0ad4e;'>⚠️ 注意（医院科室）：</strong>检测到显著的不一致偏差模式，不同浓度区间的表现差异明显。建议检查校准曲线、线性验证结果和质控数据。"
          manufacturer_msg <- "<strong style='color: #f0ad4e;'>🔧 厂商建议：</strong>检测到显著的不一致分布变化，提示试剂盒在不同浓度区间的表现不一致。建议：1) 详细分析不同浓度区间的精密度和准确度数据；2) 检查是否存在基质效应或干扰物质的影响；3) 评估校准曲线的分段拟合是否更合适；4) 考虑是否需要针对不同浓度区间优化反应条件；5) 加强该批次产品的质量监控。"
        }
      }
      # 情况3：只有显著升高（无显著降低）
      else if (n_significant_up > 0 && n_significant_down == 0) {
        conclusion <- paste0("该batch的", paste0(significant_up_quantiles, collapse="、"), 
          "分位数显著升高", 
          if (n_significant_up >= 4) "，提示存在系统性正偏倚" 
          else if (n_significant_up >= 2) "，提示存在明显的正偏倚趋势"
          else paste0("，共", n_significant_up, "个分位数"),
          "。")
        conclusion_details <- paste0("显著的升高变化集中在", paste0(significant_up_quantiles, collapse="、"), 
          "分位数", 
          if (length(up_quantiles) > 0 && !all(up_quantiles %in% significant_up_quantiles)) {
            paste0("；同时", paste0(setdiff(up_quantiles, significant_up_quantiles), collapse="、"), 
                   "分位数也有非显著的升高")
          } else "",
          "。")
        warning_msg <- if (n_significant_up >= 4) {
          "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到系统性正偏倚，可能原因包括：试剂批号更换、校准偏移、仪器漂移或质控品异常。建议立即检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
        } else {
          "<strong style='color: #f0ad4e;'>⚠️ 注意（医院科室）：</strong>检测到显著的正偏倚，建议检查试剂批号、校准状态和质控结果。"
        }
        manufacturer_msg <- if (n_significant_up >= 4) {
          "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到严重的系统性正偏倚，提示该批次试剂可能存在质量问题。建议：1) 检查该批次的生产记录，重点关注校准品配制、抗体效价、反应体系pH值等关键参数；2) 对比该批次与正常批次的生产工艺差异；3) 评估是否需要召回该批次产品；4) 加强该批次的质量追溯和留样检测。"
        } else {
          "<strong style='color: #f0ad4e;'>🔧 厂商建议：</strong>检测到显著的正偏倚，提示该批次试剂可能存在批间一致性问题。建议：1) 检查该批次与参考批次的校准品赋值是否一致；2) 评估抗体/抗原的批间效价差异；3) 检查生产过程中的关键控制点；4) 加强该批次产品的质量监控。"
        }
      }
      # 情况4：只有显著降低（无显著升高）
      else if (n_significant_down > 0 && n_significant_up == 0) {
        conclusion <- paste0("该batch的", paste0(significant_down_quantiles, collapse="、"), 
          "分位数显著降低", 
          if (n_significant_down >= 4) "，提示存在系统性负偏倚" 
          else if (n_significant_down >= 2) "，提示存在明显的负偏倚趋势"
          else paste0("，共", n_significant_down, "个分位数"),
          "。")
        conclusion_details <- paste0("显著的降低变化集中在", paste0(significant_down_quantiles, collapse="、"), 
          "分位数", 
          if (length(down_quantiles) > 0 && !all(down_quantiles %in% significant_down_quantiles)) {
            paste0("；同时", paste0(setdiff(down_quantiles, significant_down_quantiles), collapse="、"), 
                   "分位数也有非显著的降低")
          } else "",
          "。")
        warning_msg <- if (n_significant_down >= 4) {
          "<strong style='color: #d9534f;'>⚠️ 警告（医院科室）：</strong>检测到系统性负偏倚，可能原因包括：试剂批号更换、校准偏移、仪器漂移或质控品异常。建议立即检查试剂批号、校准记录和质控结果，必要时重新校准或更换试剂。"
        } else {
          "<strong style='color: #f0ad4e;'>⚠️ 注意（医院科室）：</strong>检测到显著的负偏倚，建议检查试剂批号、校准状态和质控结果。"
        }
        manufacturer_msg <- if (n_significant_down >= 4) {
          "<strong style='color: #d9534f;'>🔧 厂商建议：</strong>检测到严重的系统性负偏倚，提示该批次试剂可能存在质量问题。建议：1) 检查该批次的生产记录，重点关注校准品配制、抗体效价、反应体系pH值等关键参数；2) 对比该批次与正常批次的生产工艺差异；3) 评估是否需要召回该批次产品；4) 加强该批次的质量追溯和留样检测。"
        } else {
          "<strong style='color: #f0ad4e;'>🔧 厂商建议：</strong>检测到显著的负偏倚，提示该批次试剂可能存在批间一致性问题。建议：1) 检查该批次与参考批次的校准品赋值是否一致；2) 评估抗体/抗原的批间效价差异；3) 检查生产过程中的关键控制点；4) 加强该批次产品的质量监控。"
        }
      }
      # 情况5：无显著变化（只有非显著变化或正常）
      else {
        if (n_up > 0 || n_down > 0) {
          change_desc <- c()
          if (length(up_quantiles) > 0) {
            change_desc <- c(change_desc, paste0(paste0(up_quantiles, collapse="、"), "分位数升高"))
          }
          if (length(down_quantiles) > 0) {
            change_desc <- c(change_desc, paste0(paste0(down_quantiles, collapse="、"), "分位数降低"))
          }
          conclusion <- paste0("该batch的", paste0(change_desc, collapse="，"), 
            "，但变化均未达到显著水平（p > 0.05）。")
          conclusion_details <- "虽然存在非显著的升高或降低趋势，但统计学上未达到显著水平，可能为正常波动范围。"
          warning_msg <- "<strong style='color: #5bc0de;'>💡 提示（医院科室）：</strong>检测到非显著的变化趋势，建议持续监控。"
          manufacturer_msg <- "<strong style='color: #5bc0de;'>🔧 厂商建议：</strong>检测到非显著的变化趋势，建议持续监控该批次产品的临床使用反馈，如变化趋势持续或扩大，需进一步评估。"
        } else {
          conclusion <- "该batch的所有分位数均保持正常，未检测到显著的系统性偏差。"
          conclusion_details <- "所有分位数的表现均在正常范围内，该批次表现稳定。"
          warning_msg <- ""
          manufacturer_msg <- "<strong style='color: #5cb85c;'>✅ 厂商反馈：</strong>该批次表现稳定，符合质量要求。建议继续保持当前生产工艺和质量控制标准。"
        }
      }
      
      # 组合最终文本
      final_text <- paste0(
        if (batch_lot_info != "" && batch_lot_info != "批号-盒号信息：未知") {
          paste0("<h6 style='margin-top: 0;'>试剂盒信息：</h6>",
                 "<p style='font-size: 0.95em; color: #666;'>", batch_lot_info, "</p>",
                 "<hr style='margin: 15px 0;'/>")
        } else {
          ""
        },
        "<h6 style='margin-top: 0;'>分位数表现：</h6>",
        "<p>", quantile_text, "。</p>",
        "<h6>综合结论：</h6>",
        "<p><strong>", conclusion, "</strong></p>",
        if (nchar(conclusion_details) > 0) {
          paste0("<p style='color: #555; font-size: 0.95em; margin-top: 8px;'>", conclusion_details, "</p>")
        } else {
          ""
        },
        if (nchar(warning_msg) > 0) {
          paste0("<div style='margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 3px;'>",
                 warning_msg, "</div>")
        } else {
          ""
        },
        if (nchar(manufacturer_msg) > 0) {
          paste0("<div style='margin-top: 10px; padding: 10px; background-color: #e7f3ff; border-left: 4px solid #337ab7; border-radius: 3px;'>",
                 manufacturer_msg, "</div>")
        } else {
          ""
        }
      )
      
      HTML(final_text)
    })
    
    # 核密度估计可视化
    output$density_plot <- plotly::renderPlotly({
      req(clicked_window_info())
      req(calculation_results())
      
      window_info <- clicked_window_info()
      results <- calculation_results()
      
      # 获取对照组数据（所有全绿batch的集合）
      control_group_original <- results$control_group_original
      
      if (length(control_group_original) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "暂无全绿batch作为对照组"))
      }
      
      manu_name <- window_info$manu_name
      win_start <- window_info$win_sample_start
      win_stop <- window_info$win_sample_stop
      
      # 获取该厂家的数据
      if (!manu_name %in% names(results$manu_result_lst)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "无法找到厂家数据"))
      }
      
      manu_result <- results$manu_result_lst[[manu_name]]
      data_subset <- manu_result$data_subset
      
      # 获取测试batch的原数据（定量结果）
      testing_sample_original <- data_subset$定量结果[win_start:win_stop]
      
      # 移除NA值
      testing_sample_original <- testing_sample_original[!is.na(testing_sample_original)]
      
      if (length(testing_sample_original) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "测试batch无有效数据"))
      }
      
      # 对照组：所有全绿batch的原数据
      de_sample_population_original <- control_group_original
      de_sample_population_original <- de_sample_population_original[!is.na(de_sample_population_original)]
      
      if (length(de_sample_population_original) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "对照组无有效数据"))
      }
      
      # 核密度估计
      # 计算合适的带宽
      tryCatch({
        bw_testing <- stats::bw.nrd0(testing_sample_original)
        bw_population <- stats::bw.nrd0(de_sample_population_original)
        bw_combined <- max(bw_testing, bw_population, na.rm = TRUE)  # 使用较大的带宽以保持一致性
        
        if (!is.finite(bw_combined) || bw_combined <= 0) {
          bw_combined <- "nrd0"  # 使用默认方法
        }
        
        # 计算密度
        density_testing <- stats::density(testing_sample_original, bw = bw_combined)
        density_population <- stats::density(de_sample_population_original, bw = bw_combined)
      }, error = function(e) {
        # 如果出错，使用默认带宽
        density_testing <<- stats::density(testing_sample_original)
        density_population <<- stats::density(de_sample_population_original)
      })
      
      # 创建plotly图表
      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          x = density_population$x,
          y = density_population$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(0, 255, 0, 0.3)",  # 绿色半透明
          line = list(color = "rgba(0, 255, 0, 0.8)", width = 2),
          name = "全绿通过数据 (de_sample_population)",
          legendgroup = "population"
        ) %>%
        plotly::add_trace(
          x = density_testing$x,
          y = density_testing$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(255, 0, 0, 0.3)",  # 红色半透明
          line = list(color = "rgba(255, 0, 0, 0.8)", width = 2),
          name = "测试窗口数据 (testing_sample)",
          legendgroup = "testing"
        ) %>%
        plotly::layout(
          title = "核密度估计 - 分布对比",
          xaxis = list(title = "定量结果（原数据）"),
          yaxis = list(title = "概率密度"),
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            yanchor = "top"
          )
        )
      
      p
    })
    
    # 协变量富集分析
    output$enrichment_table <- DT::renderDataTable({
      req(clicked_window_info())
      req(calculation_results())
      
      window_info <- clicked_window_info()
      results <- calculation_results()
      
      # 获取对照组协变量数据（所有全绿batch的集合）
      control_group_covariates <- results$control_group_covariates
      
      if (is.null(control_group_covariates) || length(control_group_covariates) == 0) {
        return(DT::datatable(data.frame(Message = "暂无全绿batch作为对照组")))
      }
      manu_name <- window_info$manu_name
      win_start <- window_info$win_sample_start
      win_stop <- window_info$win_sample_stop
      
      # 获取该厂家的数据
      if (!manu_name %in% names(results$manu_result_lst)) {
        return(DT::datatable(data.frame(Message = "无法找到厂家数据")))
      }
      
      manu_result <- results$manu_result_lst[[manu_name]]
      data_subset <- manu_result$data_subset
      
      # 获取测试batch的索引
      testing_indices <- win_start:win_stop
      
      # 获取公式变量
      formula_vars <- extract_formula_vars(results$regression_formula)
      
      # 对每个协变量进行富集分析
      # 使用所有全绿batch的集合作为对照组
      enrichment_results <- list()
      
      for (var in formula_vars) {
        if (!var %in% colnames(data_subset)) next
        
        # 测试组：当前batch的协变量
        testing_var <- data_subset[[var]][testing_indices]
        testing_var <- testing_var[!is.na(testing_var)]  # 移除NA值
        
        if (length(testing_var) == 0) {
          next  # 如果测试组没有有效数据，跳过
        }
        
        # 对照组：所有全绿batch的协变量
        if (!var %in% names(control_group_covariates)) {
          next  # 如果对照组中没有这个变量，跳过
        }
        population_var <- control_group_covariates[[var]]
        population_var <- population_var[!is.na(population_var)]  # 移除NA值
        
        if (length(population_var) == 0) {
          next  # 如果对照组没有有效数据，跳过
        }
        
        # 如果是数值型变量，进行t检验或Wilcoxon检验
        if (is.numeric(testing_var) || is.integer(testing_var)) {
          # 确保population_var也是数值型
          if (!is.numeric(population_var) && !is.integer(population_var)) {
            next  # 类型不匹配，跳过
          }
          # 检查正态性（简化处理，使用Shapiro-Wilk检验，但样本量大时可能不适用）
          if (length(testing_var) <= 5000 && length(population_var) <= 5000) {
            norm_test_testing <- tryCatch(stats::shapiro.test(testing_var)$p.value > 0.05, error = function(e) FALSE)
            norm_test_population <- tryCatch(stats::shapiro.test(population_var)$p.value > 0.05, error = function(e) FALSE)
          } else {
            norm_test_testing <- FALSE
            norm_test_population <- FALSE
          }
          
          if (norm_test_testing && norm_test_population) {
            # t检验
            test_result <- tryCatch(
              stats::t.test(testing_var, population_var),
              error = function(e) NULL
            )
            test_name <- "t检验"
          } else {
            # Wilcoxon秩和检验
            test_result <- tryCatch(
              stats::wilcox.test(testing_var, population_var),
              error = function(e) NULL
            )
            test_name <- "Wilcoxon检验"
          }
          
          if (!is.null(test_result)) {
            testing_mean_val <- mean(testing_var, na.rm = TRUE)
            population_mean_val <- mean(population_var, na.rm = TRUE)
            diff_val <- testing_mean_val - population_mean_val
            
            enrichment_results[[var]] <- data.frame(
              协变量 = var,
              类型 = "数值型",
              检验方法 = test_name,
              测试组 = paste0(round(testing_mean_val, 4)),
              对照组 = paste0(round(population_mean_val, 4)),
              差异 = paste0(round(diff_val, 4)),
              p_value = test_result$p.value,
              stringsAsFactors = FALSE
            )
          }
        } else {
          # 分类变量，使用卡方检验或Fisher精确检验
          # 确保两个变量都是因子或字符型
          if (is.factor(testing_var)) {
            testing_var <- as.character(testing_var)
          }
          if (is.factor(population_var)) {
            population_var <- as.character(population_var)
          }
          
          # 构建列联表
          testing_table <- table(testing_var, useNA = "no")
          population_table <- table(population_var, useNA = "no")
          
          # 获取所有类别
          all_levels <- unique(c(names(testing_table), names(population_table)))
          
          if (length(all_levels) < 1) {
            next  # 没有有效类别，跳过
          }
          
          if (length(all_levels) > 1) {
            # 构建列联表，确保所有level都有对应的计数（不存在则为0）
            testing_counts <- rep(0, length(all_levels))
            names(testing_counts) <- all_levels
            testing_counts[names(testing_table)] <- testing_table
            
            population_counts <- rep(0, length(all_levels))
            names(population_counts) <- all_levels
            population_counts[names(population_table)] <- population_table
            
            # 构建列联表
            contingency_table <- rbind(
              testing_counts,
              population_counts
            )
            rownames(contingency_table) <- c("testing", "population")
            colnames(contingency_table) <- all_levels
            
            # 检查是否适合卡方检验（期望频数 >= 5）
            # 先尝试计算期望频数
            expected <- NULL
            tryCatch({
              chisq_test <- stats::chisq.test(contingency_table)
              expected <- chisq_test$expected
            }, error = function(e) {
              expected <<- NULL
            })
            
            if (!is.null(expected) && all(expected >= 5, na.rm = TRUE)) {
              # 卡方检验
              test_result <- tryCatch(
                stats::chisq.test(contingency_table),
                error = function(e) NULL
              )
              test_name <- "卡方检验"
            } else {
              # Fisher精确检验（仅适用于2x2表）
              if (ncol(contingency_table) == 2 && nrow(contingency_table) == 2) {
                test_result <- tryCatch(
                  stats::fisher.test(contingency_table),
                  error = function(e) NULL
                )
                test_name <- "Fisher精确检验"
              } else {
                test_result <- NULL
                test_name <- "不适用"
              }
            }
            
            if (!is.null(test_result)) {
              # 计算每个类别的富集比例（基于已经构建好的计数）
              testing_props_full <- prop.table(testing_counts)
              population_props_full <- prop.table(population_counts)
              
              # 找到最大差异的类别
              diffs <- abs(testing_props_full - population_props_full)
              max_diff_idx <- which.max(diffs)
              max_diff_level <- names(diffs)[max_diff_idx]
              max_diff <- testing_props_full[max_diff_level] - population_props_full[max_diff_level]
              
              enrichment_results[[var]] <- data.frame(
                协变量 = var,
                类型 = "分类变量",
                检验方法 = test_name,
                测试组 = paste0(round(testing_props_full[max_diff_level] * 100, 2), "%"),
                对照组 = paste0(round(population_props_full[max_diff_level] * 100, 2), "%"),
                差异 = paste0(round(max_diff * 100, 2), "%"),
                p_value = test_result$p.value,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
      
      if (length(enrichment_results) == 0) {
        return(DT::datatable(data.frame(Message = "无法进行协变量富集分析"), rownames = FALSE))
      }
      
      # 合并结果
      # 现在所有数据框都有相同的列结构，可以直接使用bind_rows
      enrichment_df <- dplyr::bind_rows(enrichment_results)
      
      if (is.null(enrichment_df) || nrow(enrichment_df) == 0) {
        return(DT::datatable(data.frame(Message = "合并结果失败"), rownames = FALSE))
      }
      enrichment_df$p_value_adjusted <- stats::p.adjust(enrichment_df$p_value, method = "BH")
      enrichment_df$显著 <- ifelse(enrichment_df$p_value_adjusted < 0.05, "是", "否")
      enrichment_df <- enrichment_df[order(enrichment_df$p_value_adjusted), ]
      
      # 格式化数值
      enrichment_df$p_value <- round(enrichment_df$p_value, 6)
      enrichment_df$p_value_adjusted <- round(enrichment_df$p_value_adjusted, 6)
      
      DT::datatable(
        enrichment_df,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          order = list(list(6, 'asc'))  # 按调整后的p值排序
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "显著",
          target = "row",
          backgroundColor = DT::styleEqual("是", "rgba(255, 0, 0, 0.1)")
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
