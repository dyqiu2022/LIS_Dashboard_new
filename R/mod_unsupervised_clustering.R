#' unsupervised_clustering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom dplyr %>% arrange group_by reframe select mutate distinct filter
#' @importFrom rlang sym .data
#' @importFrom DT renderDataTable datatable formatStyle dataTableOutput
#' @importFrom bslib card_body
#' @importFrom shiny validate need
mod_unsupervised_clustering_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "词向量无监督聚类",
    fillable = TRUE,
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "无监督聚类参数",
        width = "360px",
        bg = "#f8f9fa",
        open = "desktop",
        uiOutput(ns("choose_grouping_col2")),
        uiOutput(ns("diag_kmeans_ui"))
      ),
      card(
        full_screen = TRUE,
        height = "100%",
        card_header("当前无监督聚类情况"),
        card_body(
          padding = 0,
          DT::dataTableOutput(ns("diag_count2"), height = "100%", fill = TRUE)
        )
      )
    )
  )
}

#' unsupervised_clustering Server Functions
#'
#' @noRd
mod_unsupervised_clustering_server <- function(id, global_store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    grouping_table2 <- reactiveVal(NULL)
    embedding_matrix <- reactiveVal(NULL)
    data2 <- reactiveVal(NULL)

    observe({
      req(global_store[["reactive_data_na_"]])
      validate(need(nrow(global_store[["reactive_data_na_"]]) > 0, message = ""))
      req(input$selected_grouping_col2)

      data <- global_store[["reactive_data_na_"]]
      if (!"类别_无监督" %in% colnames(data)) data$`类别_无监督` <- ""

      new_table <- data %>%
        dplyr::group_by(!!rlang::sym(input$selected_grouping_col2), `类别_无监督`) %>%
        dplyr::reframe(数量 = dplyr::n()) %>%
        dplyr::arrange(-数量)

      new_table <- new_table[, c(input$selected_grouping_col2, "数量", "类别_无监督")]
      grouping_table2(new_table)
    })

    output$diag_count2 <- DT::renderDataTable({
      req(grouping_table2())
      data <- grouping_table2()
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

    output$choose_grouping_col2 <- renderUI({
      req(global_store[["reactive_data_na_"]])
      cols <- colnames(global_store[["reactive_data_na_"]])
      choices <- cols[cols != "类别_无监督"]
      selected <- if ("临床诊断" %in% choices) "临床诊断" else choices[[1]]
      selectInput(ns("selected_grouping_col2"), "无监督聚类列", choices = choices, selected = selected, width = "100%")
    })

    observeEvent(input$selected_grouping_col2, {
      output$diag_kmeans_ui <- renderUI({
        div(class = "pt-2", actionButton(ns("load_model_worker"), "进行分词和词语向量化", width = "100%"))
      })
    })

    observeEvent(input$load_model_worker, {
      output$diag_kmeans_ui <- renderUI({
        tagList(
          numericInput(ns("cluster_num"), "输入K-Means的Cluster数量:", value = 30, min = 0, max = 100, width = "100%"),
          div(class = "pt-2", actionButton(ns("Start_kmeans"), "开始进行kmeans聚类", width = "100%")),
          div(class = "pt-2", uiOutput(ns("merge_result_ui2")))
        )
      })

      showNotification("正在载入词向量模型...", type = "message")
      word_matrix_path <- app_sys("extdata", "original_word_matrix.rds")
      validate(need(word_matrix_path != "", message = "找不到 original_word_matrix.rds（请确认已放入 inst/extdata/）"))
      embedding_matrix(readRDS(word_matrix_path))

      showNotification("正在进行分词worker初始化...", type = "message")
      custom_words_path <- app_sys("extdata", "custom_words.txt")
      validate(need(custom_words_path != "", message = "找不到 custom_words.txt（请确认已放入 inst/extdata/）"))
      wk_ <- jiebaR::worker(type = "tag", user = custom_words_path)

      withProgress(message = "正在进行分词中...", value = 0, {
        total <- nrow(grouping_table2())

        jieba_tagging_or_not <- function(word_, jieba, embedding_matrix) {
          incProgress(1 / total, detail = paste(""))
          if (word_ %in% rownames(embedding_matrix)) {
            return(c("1" = word_))
          } else {
            result <- tryCatch({ jiebaR::tagging(word_, jieba) }, error = function(e) character(0))
            # 防止 Windows CRLF 行尾导致 tag 末尾带入 '\r'
            if (length(result) > 0 && !is.null(names(result))) {
              names(result) <- sub("\r$", "", names(result))
            }
            return(result)
          }
        }

        grouping_table2() %>%
          dplyr::mutate(id = c(1:nrow(.))) %>%
          dplyr::mutate(words = purrr::map(!!rlang::sym(input$selected_grouping_col2), jieba_tagging_or_not, jieba = wk_, embedding_matrix = embedding_matrix())) %>%
          dplyr::mutate(word_tag = purrr::map(words, tibble::enframe, name = "tag", value = "word")) %>%
          dplyr::mutate(word_tag_show = purrr::map(words, named_lst_paste)) -> data2_
      })

      merge(grouping_table2(), data2_[, c(input$selected_grouping_col2, "word_tag_show")], by = input$selected_grouping_col2) %>%
        grouping_table2(.)

      withProgress(message = "正在进行向量化...", value = 0, {
        total <- nrow(data2_)
        get_vec_frame <- function(word_tag, embedding_matrix) {
          incProgress(1 / total, detail = paste(""))
          word_tag %>% dplyr::filter(.data$tag %in% c("1")) -> word_tag_filtered
          vec_lst <- lapply(word_tag_filtered$word, get_vec_, embedding_matrix)
          vec_lst <- vec_lst[which(!is.na(vec_lst))]
          Reduce("+", vec_lst) / length(vec_lst)
        }
        data2_$word_vec <- lapply(data2_$word_tag, get_vec_frame, embedding_matrix = embedding_matrix())
      })

      data2(data2_)
    })

    observeEvent(input$Start_kmeans, {
      req(data2())
      k <- input$cluster_num
      data2_ <- data2()

      max_sample_size <- max(data2_$数量)
      n <- log(max_sample_size, base = max(nrow(data2_) / (2 * k), 3))
      data2_$sample_size_stand <- data2_$数量^(1 / n) %>% ceiling()
      data2_expanded <- data2_ %>% tidyr::uncount(weights = sample_size_stand, .remove = FALSE, .id = "copy_id")
      matrix_data <- do.call(rbind, data2_expanded$word_vec)

      set.seed(123)
      kmeans_result <- kmeans_with_progress(matrix_data, centers = k, nstart = 20, iter.max = 30)

      showNotification("正在整理K-means计算结果...", type = "message")
      cluster <- kmeans_result$cluster %>% unname()
      data2_expanded$cluster <- NA
      data2_expanded[which(lengths(data2_expanded$word_vec) > 0), ]$cluster <- cluster
      data2_expanded <- data2_expanded %>% dplyr::select(-copy_id, -word_vec) %>% dplyr::distinct()

      cluster_centers <- kmeans_result$centers

      withProgress(message = "正在寻找中心临近关键词...", value = 0, {
        find_closest_words_fast <- function(centers, embedding_matrix, top_n = 3) {
          incProgress(0 / 4, detail = paste("归一化向量"))
          norm_embed <- t(apply(embedding_matrix, 1, function(x) x / sqrt(sum(x^2))))
          incProgress(1 / 4, detail = paste("归一化向量"))
          norm_centers <- t(apply(centers, 1, function(x) x / sqrt(sum(x^2))))
          incProgress(2 / 4, detail = paste("余弦相似度计算"))
          similarity_matrix <- norm_embed %*% t(norm_centers)
          incProgress(3 / 4, detail = paste("寻找top_n词语"))
          lapply(1:nrow(centers), function(i) {
            sorted_indices <- order(similarity_matrix[, i], decreasing = TRUE)
            rownames(embedding_matrix)[sorted_indices[1:top_n]]
          })
        }

        closest_words_fast <- find_closest_words_fast(cluster_centers, embedding_matrix())
        incProgress(4 / 4, detail = paste("完成"))
      })

      closest_words_lst <- lapply(closest_words_fast, paste, collapse = "、") %>% unlist()
      center_words <- data.frame("聚类中心临近诊断" = closest_words_lst, cluster = c(1:length(closest_words_lst)))
      data3 <- merge(data2_expanded, center_words, by = "cluster")

      distances <- sqrt(rowSums((matrix_data - kmeans_result$centers[kmeans_result$cluster, ])^2))
      avg_distance <- tapply(distances, kmeans_result$cluster, mean)
      sample_size <- tapply(data2_expanded$数量, data2_expanded$cluster, sum)
      diag_size <- tapply(data2_expanded$数量, data2_expanded$cluster, length)

      dispersion_df <- data.frame(
        cluster = as.numeric(names(avg_distance)),
        avg_distance = avg_distance,
        diag_size = diag_size,
        sample_size_ = sample_size
      ) %>%
        dplyr::arrange(avg_distance)

      dispersion_df$distances_rank <- 1:nrow(dispersion_df)
      max_idx <- which.max(dispersion_df$distances_rank)
      dispersion_df$distances_rank[max_idx] <- dispersion_df$distances_rank[max_idx] * 1.5

      dispersion_df <- dispersion_df %>% dplyr::arrange(-diag_size)
      dispersion_df$diag_size_rank <- 1:nrow(dispersion_df)
      max_idx <- which.max(dispersion_df$diag_size_rank)
      dispersion_df$diag_size_rank[max_idx] <- dispersion_df$diag_size_rank[max_idx] * 1.5

      dispersion_df <- dispersion_df %>% dplyr::arrange(-sample_size_)
      dispersion_df$sample_size_rank <- 1:nrow(dispersion_df)
      max_idx <- which.max(dispersion_df$sample_size_rank)
      dispersion_df$sample_size_rank[max_idx] <- dispersion_df$sample_size_rank[max_idx] * 1.5

      dispersion_df$rank_sum <- dispersion_df$distances_rank + dispersion_df$diag_size_rank + dispersion_df$sample_size_rank
      dispersion_df <- dispersion_df %>% dplyr::arrange(rank_sum)
      dispersion_df$sum_rank <- 1:nrow(dispersion_df)

      data4 <- merge(data3, dispersion_df, by = "cluster")
      data4 <- data4[, c("sum_rank", "聚类中心临近诊断", "sample_size_", input$selected_grouping_col2, "数量", "word_tag_show", "distances_rank", "diag_size_rank", "sample_size_rank")]
      colnames(data4) <- c(
        "类综合排名",
        "类别_无监督",
        "类别数量",
        input$selected_grouping_col2,
        paste0(input$selected_grouping_col2, "数量"),
        "分词与词性",
        "类紧密度排名",
        paste0(input$selected_grouping_col2, "丰富度排名"),
        "样本数量排名"
      )

      data4 %>% grouping_table2(.)
      showNotification("完成K-means聚类", type = "message")
      output$merge_result_ui2 <- renderUI(div(class = "pt-2", actionButton(ns("merge_result2"), "聚类结果写入数据", width = "100%")))
    })

    observeEvent(input$merge_result2, {
      req(global_store[["reactive_data_na_"]])
      req(grouping_table2())
      req(input$selected_grouping_col2)

      new_reactive_data <- global_store[["reactive_data_na_"]] %>% dplyr::select(-dplyr::any_of("类别_无监督"))
      saved_col_names <- colnames(new_reactive_data)
      new_reactive_data <- merge(new_reactive_data, grouping_table2()[, c(input$selected_grouping_col2, "类别_无监督")], by = input$selected_grouping_col2, all.x = TRUE)
      new_reactive_data <- new_reactive_data[, c(saved_col_names, "类别_无监督")]

      if (length(which(is.na(new_reactive_data$`类别_无监督`))) > 0) {
        new_reactive_data[which(is.na(new_reactive_data$`类别_无监督`)), ]$`类别_无监督` <- "未知"
      }

      global_store[["reactive_data_na_"]] <- new_reactive_data
    })
  })
}

