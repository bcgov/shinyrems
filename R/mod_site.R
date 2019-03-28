# Module UI

#' @title   mod_site_ui and mod_site_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_site
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_site_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      tabsetPanel(
        tabPanel(title = "Site Map"),
        tabPanel(title = "Site Table")
      )
    )
  )
}

# Module Server

#' @rdname mod_site
#' @export
#' @keywords internal

mod_site_server <- function(input, output, session){
  ns <- session$ns
}

## To be copied in the UI
# mod_site_ui("site_ui_1")

## To be copied in the server
# callModule(mod_site_server, "site_ui_1")

