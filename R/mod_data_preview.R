# Module UI

#' @title   mod_data_preview_ui and mod_data_preview_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_preview
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("preview_table")),
    downloadButton(ns("dl_data_handler"), label = NULL,
                   style = "visibility: hidden;")
  )
}

# Module Server

#' @rdname mod_data_preview
#' @export
#' @keywords internal

mod_data_preview_server <- function(input, output, session, run_mode, data){
  ns <- session$ns

  output$preview_table <- renderUI({
    if(is.character(data)){
      return(error_text(data))
    }
    tagList(
      button(ns('dl_data'), label = "Download Raw Data"),
      br2(),
      ems_table_output(ns('data_table'))
    )
  })

  output$data_table <- DT::renderDT({
    ems_data_table(data)
  })

  observeEvent(input$dl_data, {
    shinyjs::runjs(click_js(ns("dl_data_handler")))
  })

  output$dl_data_handler <- downloadHandler(
    filename = function() "ems_data.csv",
    content = function(file) {
      readr::write_csv(data, file)
    })
}

## To be copied in the UI
# mod_data_preview_ui("data_preview_ui_1")

## To be copied in the server
# callModule(mod_data_preview_server, "data_preview_ui_1")

