# Module UI
  
#' @title   mod_refine_view_ui and mod_refine_view_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_refine_view
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_refine_view_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_refine_view
#' @export
#' @keywords internal
    
mod_refine_view_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_refine_view_ui("refine_view_ui_1")
    
## To be copied in the server
# callModule(mod_refine_view_server, "refine_view_ui_1")
 
