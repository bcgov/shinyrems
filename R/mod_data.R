# Module UI

#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("dataset"), label = "Select dataset",
                 choices = datasets,
                 selected = "demo", inline = TRUE),
    # uiOutput(ns("check_data_ui")),
    shinyjs::hidden(div(id = ns("div_data_find"),
        tags$label("Select site(s) or"),
        actionLink(ns("search_map"), label = "find sites on map") %>%
          bsplus::bs_attach_modal(id_modal = ns("modal_map")),
        modal_sitemap(ns),
        checkboxInput(ns("check_permit"),
                      label = "Filter by Permit Number",
                      value = FALSE),
        uiOutput(ns("ui_permit")),
        radioButtons(ns("site_type"), label = NULL,
                     choices = c("Monitoring Location", "EMS ID"),
                     selected = "Monitoring Location", inline = TRUE),
        uiOutput(ns("ui_site")),
        tags$label("Select Parameter(s)"),
        radioButtons(ns("parameter_type"), label = NULL,
                     choices = c("Parameter Name", "Parameter Code"),
                     selected = "Parameter Name", inline = TRUE),
        uiOutput(ns("ui_parameter")),
        uiOutput(ns("ui_date")))),
    shinyjs::hidden(div(id = ns("div_data_upload"),
        radioButtons(ns("data_type"), label = "Data format",
                     choices = c("tidy" = "Tidied EMS Data", "raw" = "Raw EMS Data"),
                     selected = "tidy"),
        fileInput(ns("upload_data"),
                  buttonLabel = span(tagList(icon("upload"), "csv")),
                  label = "",
                  placeholder = "Upload your own dataset",
                  accept = c('.csv')),
        button(ns('dl_template'), label = "Download Template"),
        downloadButton(ns("dl_template_handler"), label = NULL,
                       style = "visibility: hidden;")))

  )
}

# Module Server

#' @rdname mod_data
#' @export
#' @keywords internal

mod_data_server <- function(input, output, session){
  ns <- session$ns

  ########## ---------- dataset ---------- ##########
  ns <- session$ns

  observeEvent(input$dataset, {
    dataset <- input$dataset
    check <- check_data_progress(dataset)
    if(check[1] != "done")
      showModal(data_download_modal(check[1], check[2], ns))
  })

  # observe({
  #   dataset <- input$dataset
  #   if(dataset %in% c("demo", "upload"))
  #     return()
  #   # check_data <- check_data_progress(dataset)
  #   check_data <- list(check = "done",
  #                      which = dataset)
  #   check <- check_data[["check"]]
  #   which <- check_data[["which"]]
  #   if(check == "done")
  #     return()
  #   output$check_data_ui <- renderUI({
  #     showModal(data_download_ui(which, check, ns))
  #   })
  # })
  #
  # observeEvent(input$no_download, {
  #   updateRadioButtons(session, "dataset", selected = "demo")
  #   removeModal()
  # })
  #
  # observeEvent(input$no_update, {
  #   removeModal()
  # })
  #
  # observeEvent(input$yes_download, {
  #   withCallingHandlers({
  #     shinyjs::html("download_text", "")
  #     download_data(input$dataset, session, "download_progress")
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "download_text", html = m$message, add = TRUE)
  #   })
  #   removeModal()
  # })
}

## To be copied in the UI
# mod_data_ui("data_ui_1")

## To be copied in the server
# callModule(mod_data_server, "data_ui_1")

