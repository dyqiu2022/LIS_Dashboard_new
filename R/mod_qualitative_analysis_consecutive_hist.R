#' qualitative_analysis_consecutive_hist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qualitative_analysis_consecutive_hist_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' qualitative_analysis_consecutive_hist Server Functions
#'
#' @noRd 
mod_qualitative_analysis_consecutive_hist_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_qualitative_analysis_consecutive_hist_ui("qualitative_analysis_consecutive_hist_1")
    
## To be copied in the server
# mod_qualitative_analysis_consecutive_hist_server("qualitative_analysis_consecutive_hist_1")
