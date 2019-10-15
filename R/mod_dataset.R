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
                 selected = "demo", inline = TRUE)
  )
}

# Module Server

#' @rdname mod_launch
#' @export
#' @keywords internal

mod_dataset_server <- function(input, output, session){
  ns <- session$ns

  check_data <- reactive({
    dataset <- input$dataset
    withProgress(Sys.sleep(2), value = 0.5,
                              message = glue("Checking that data exists..."))
    download <- FALSE
    if(download){
      return(showModal(modal_data(input$dataset, update = FALSE, ns)))
    }
    withProgress(Sys.sleep(2), value = 0.5,
                 message = glue("Checking for updates..."))
    update <- TRUE
    if(update){
      return(showModal(modal_data(input$dataset, update = TRUE, ns)))
    }
    update
    # withProgress(check_data_exists(input$dataset), value = 0.5,
    #              message = glue("Checking for updates to
    #                             '{input$dataset}' dataset..."))
    # withProgress(check_data_update(input$dataset), value = 0.5,
    #              message = glue("Checking that '{input$dataset}' dataset
    #                             has been downloaded..."))

  })

  dataset_rv <- reactiveValues(done = "demo")

  observeEvent(input$dataset, {
    if(input$dataset %in% c("demo", "upload")){
      dataset_rv$done <- input$dataset
      return()
    }
    dataset_rv$done <- "none"
    data <- check_data()
    if(is.data.frame(data)){
      dataset_rv$done <- input$dataset
    }
  })

  observeEvent(input$no_download, {
    dataset_rv$done <- "demo"
    updateRadioButtons(session, "dataset", selected = "demo")
    removeModal()
  })

  observeEvent(input$no_update, {
    removeModal()
    dataset_rv$done <-  input$dataset
  })

  foo <- function() {
    message("one")
    Sys.sleep(0.5)
    message("two")
  }

  output$download_text <- renderText({"hi"})

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
    dataset_rv$done <- input$dataset
  })

  return(reactive(dataset_rv$done))
}


