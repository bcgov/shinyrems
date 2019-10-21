# Module UI
  
#' @title   mod_standardise_ui and mod_standardise_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_standardise
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_standardise_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_standardise
#' @export
#' @keywords internal
    
mod_standardise_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_standardise_ui("standardise_ui_1")
    
## To be copied in the server
# callModule(mod_standardise_server, "standardise_ui_1")
 
