# Module UI

#' @title   mod_data_check_ui and mod_data_check_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_check
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_check_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("check_data"))
  )
}

# Module Server

#' @rdname mod_data_check
#' @export
#' @keywords internal

mod_data_check_server <- function(input, output, session, dataset, check, which){
  ns <- session$ns

  observe({
    check <- check()
    dataset <- dataset()
    which <- which()

    if(check == "download"){
      output$check_data <- renderUI({
        data_download_ui(which, ns)
      })
    }
    if(check == "update"){
      output$check_data <- renderUI({
        data_upload_ui(which, ns)

      })
    }
  })

  check_rv <- reactiveValues(check = NULL)

  observeEvent(input$no_download, {
    check_rv$check <- "no_download"
  })

  observeEvent(input$no_update, {
    check_rv$check <- "no_update"
  })

  foo <- function() {
    message("one")
    Sys.sleep(0.5)
    message("two")
  }

  observeEvent(input$yes_download, {
    withCallingHandlers({
      shinyjs::html("download_text", "")
      foo()
      # download_data(input$dataset)
    },
    message = function(m) {
      shinyjs::html(id = "download_text", html = m$message, add = TRUE)
    })
    # removeModal()
    check_rv$check <- "yes_download"
  })

  return(reactive(check_rv$check))
}

## To be copied in the UI
# mod_data_check_ui("data_check_ui_1")

## To be copied in the server
# callModule(mod_data_check_server, "data_check_ui_1")

