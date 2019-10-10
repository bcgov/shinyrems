# Module UI

#' @title   mod_refine_sidebar_ui and mod_refine_sidebar_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_refine_sidebar
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_refine_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("sample_class"), label = "Sample Class",
                 choices = c("Regular", "Replicate", "Average")),
    radioButtons(ns("sample_state"), label = "Sample State",
                 choices = c(""))

  )
}

# Module Server

#' @rdname mod_refine_sidebar
#' @export
#' @keywords internal

mod_refine_sidebar_server <- function(input, output, session){
  ns <- session$ns
}

## To be copied in the UI
# mod_refine_sidebar_ui("refine_sidebar_ui_1")

## To be copied in the server
# callModule(mod_refine_sidebar_server, "refine_sidebar_ui_1")

