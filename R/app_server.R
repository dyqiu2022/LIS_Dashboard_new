#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # 生成状态总线
  global_store <- reactiveValues()


  # Your application server logic
  mod_data_upload_server("origin", global_store)
  mod_keyword_clustering_server("origin", global_store)
  mod_unsupervised_clustering_server("origin", global_store)
  mod_analysis_tools_sidebar_server("origin", global_store)
  mod_quantitative_discretization_server("origin", global_store)
  mod_qualitative_analysis_server("origin", global_store)
  mod_quantitative_analysis_server("origin", global_store)
  mod_show_filtered_data_server("origin", global_store)
}
