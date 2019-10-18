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
    hide("div_data_find")
    dataset <- input$dataset

    check <- check_data_progress(dataset)
    # check <- c("done", which)
    if(check[1] != "done"){
      showModal(data_download_modal(check[1], check[2], ns))
      return()
    }
    show("div_data_find")
  })

  observeEvent(input$no_download, {
    updateRadioButtons(session, "dataset", selected = "demo")
    removeModal()
    show("div_data_find")
  })

  observeEvent(input$no_update, {
    removeModal()
    show("div_data_find")
  })

  observeEvent(input$yes_download, {
    withCallingHandlers({
      shinyjs::html("download_text", "")
      download_data(input$dataset, session, "download_progress")
    },
    message = function(m) {
      shinyjs::html(id = "download_text", html = m$message, add = TRUE)
    })
    removeModal()
    show("div_data_find")
  })

  lookup <- reactive({
    get_lookup(input$dataset)
  })

  lookup_location <- reactive({
    req(lookup())
    get_lookup_location(lookup())
  })

  get_permits <- reactive({
    req(input$check_permit)
    permits(lookup())
  })

  get_sites <- reactive({
    x <- permit_sites(input$permit, lookup())
    translate_sites(x, lookup(), input$site_type)
  })

  get_site_locations <- reactive({
    lookup <- lookup_location()
    lookup[lookup$EMS_ID %in% get_sites(),]
  })

  get_parameters <- reactive({
    x <- site_parameters(input$site, lookup())
    translate_parameters(x, lookup(), input$parameter_type)
  })

  get_data <- reactive({
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    ems_data_progress(input$dataset, input$parameter, input$site,
                      input$date_range[1], input$date_range[2])
  })

  output$ui_site_modal <- renderUI({
    select_input_x(ns("site_modal"),
                   label = "Selected Site(s)",
                   choices = input$site,
                   selected = input$site)
  })

  output$ui_wsgroup <- renderUI({
    selectInput(ns("wsgroup"), label = "Zoom to watershed group",
                choices = c(sort(watershed_groups$WATERSHED_GROUP_NAME), ""),
                selected = "")
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"), label = "Permit number:",
                   choices = c(get_permits(), ""),
                   selected = "")
  })

  output$ui_site <- renderUI({
    select_input_x(ns("site"), label = NULL,
                   choices = c(get_sites(), ""),
                   selected = "")
  })

  output$ui_parameter <- renderUI({
    select_input_x(ns("parameter"),
                   label = NULL,
                   choices = c(get_parameters(), ""),
                   selected = "")
  })

  output$ui_date <- renderUI({
    req(input$parameter)
    req(input$site)
    dates <- date_range(input$site, input$parameter, lookup())
    dateRangeInput(ns("date_range"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })

  observeEvent(input$search_map, {
    shinyBS::toggleModal(session, ns("modal_map"), toggle = "open")
  })

  output$site_map <- leaflet::renderLeaflet({
    ems_leaflet(watershed_groups, get_site_locations(), input$site_type)
  })

  observe({
    req(input$wsgroup)
    zoom_to("site_map", input$wsgroup)
  })

  observeEvent(input$site_map_marker_click, {
    sites <- c(input$site_map_marker_click$id, input$site)
    updateSelectizeInput(session, ns("site_modal"), selected = sites)
  })

  observeEvent(input$site_map_shape_click, {
    ws <- input$site_map_shape_click$id
    updateSelectizeInput(session, ns("wsgroup"), selected = ws)
  })
}

## To be copied in the UI
# mod_data_ui("data_ui_1")

## To be copied in the server
# callModule(mod_data_server, "data_ui_1")

