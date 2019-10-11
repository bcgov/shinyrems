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

  check_update <- reactive({
    req(input$dataset)
    if(input$dataset %in% c("upload", "demo"))
      return()
    check_data_update(input$dataset)
  })

  check_exists <- reactive({
    req(input$dataset)
    if(input$dataset %in% c("upload", "demo"))
      return()
    check_data_exists(input$dataset)
  })

  observe({
    req(check_update())
    req(check_exists())

    if(check_exists())
      return(showModal(modal_data(input$dataset, FALSE, ns)))

    if(check_update())
      return(showModal(modal_update(input$dataset, TRUE, ns)))
  })

  observeEvent(input$yes, {
    output$download_text_ui <- renderUI({
      textOutput(ns("download_text"))
    })
    download_dataset(input$dataset)
  })

  output$download_text <- renderText({
    glue("Downloading {input$dataset} dataset. This could take a while...")

  })

  observeEvent(input$no, {
    updateRadioButtons("dataset", selected = "demo")
    removeModal()
  })

  return(reactive(input$dataset))
}


