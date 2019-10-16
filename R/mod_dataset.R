# Module UI

#' @title   mod_launch_ui and mod_launch_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_launch
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("dataset"), label = "Select dataset",
                 choices = run_modes,
                 selected = "demo", inline = TRUE),
    uiOutput(ns("check_data_ui"))
  )
}

# Module Server

#' @rdname mod_launch
#' @export
#' @keywords internal

mod_dataset_server <- function(input, output, session){
  ns <- session$ns

  observe({
    dataset <- input$dataset
    if(dataset %in% c("demo", "upload"))
      return()

    check_data <- check_data_progress(dataset)
    check <- check_data[["check"]]
    which <- check_data[["which"]]

    if(check == "done")
      return()

    output$check_data_ui <- renderUI({
      showModal(data_download_ui(which, check, ns))
    })
  })

  observeEvent(input$no_download, {
    updateRadioButtons(session, "dataset", selected = "demo")
    removeModal()
  })

  observeEvent(input$no_update, {
    removeModal()
  })

  foo <- function() {
    message("one")
    Sys.sleep(0.5)
    message("two")
  }

  observeEvent(input$yes_download, {
    # output$download_progress_ui <- renderUI({
    #   shinyWidgets::progressBar(
    #     id = ns("download_progress"),
    #     value = 0,
    #     title = "",
    #     display_pct = TRUE
    #   )
    # })
    withCallingHandlers({
      shinyjs::html("download_text", "")
      download_data(input$dataset, session, "download_progress")
    },
    message = function(m) {
      shinyjs::html(id = "download_text", html = m$message, add = TRUE)
    })
    removeModal()
  })

  return(reactive({input$dataset}))
}


