# Module UI
  
#' @title   mod_ems_ui and mod_ems_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ems
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ems_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_ems
#' @export
#' @keywords internal
    
mod_ems_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_ems_ui("ems_ui_1")
    
## To be copied in the server
# callModule(mod_ems_server, "ems_ui_1")
 
