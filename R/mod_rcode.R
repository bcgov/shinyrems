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
          uiOutput(ns('rcode'))
    )
  ))
}

# Module Server

#' @rdname mod_rcode
#' @export
#' @keywords internal

mod_rcode_server <- function(input, output, session, data, tidy,
                             clean, outlier, results){
  ns <- session$ns

  dataset <- getShinyOption("dataset", "demo")

  output$rcode <- renderUI({
    tagList(
      rcode_head(dataset),
      br2(),
      data$rcode(),
      br2(),
      tidy$rcodetidy(),
      br2(),
      outlier$rcodeclean(),
      br2(),
      outlier$rcodeoutlier(),
      br2(),
      results$rcodeplot(),
      br2(),
      results$rcodetable()
    )
  })
}

## To be copied in the UI
# mod_rcode_ui("rcode_ui_1")

## To be copied in the server
# callModule(mod_rcode_server, "rcode_ui_1")

