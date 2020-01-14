# Module UI

#' @title   mod_rcode_ui and mod_rcode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_rcode
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_rcode_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput("ui_help"),
    div(id = 'codes',
        wellPanel(
          uiOutput(ns('code_head')),
          br(),
          uiOutput(ns("code_data")),
          br()
    )
  ))
}

# Module Server

#' @rdname mod_rcode
#' @export
#' @keywords internal

mod_rcode_server <- function(input, output, session, tidy,
                             stand, clean, outlier, results){
  ns <- session$ns

  dataset <- getShinyOption("dataset", "demo")

  output$code_head <- renderUI({
    if(dataset == "upload")
      return()
    l1 <- "install.packages('rems')"
    l2 <- "library(rems)"
    l3 <- "library(wqbc)"
    l4 <- "library(ggplot2)"
    if(dataset == "upload") {
      l5 <- "library(readr)"} else {
        l5 <- NULL}
    HTML(paste(l1, l2, l3, l4, l5, sep = "<br/>"))
  })

  output$code_data <- renderUI({

  })
}

## To be copied in the UI
# mod_rcode_ui("rcode_ui_1")

## To be copied in the server
# callModule(mod_rcode_server, "rcode_ui_1")

