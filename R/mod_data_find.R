# Module UI

#' @title   mod_data_find_ui and mod_data_find_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_find
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_find_ui <- function(id){
  ns <- NS(id)
  tagList(

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
    uiOutput(ns("ui_date")))
}

# Module Server

#' @rdname mod_data_find
#' @export
#' @keywords internal

mod_data_find_server <- function(input, output, session, dataset){
  ns <- session$ns

  observe({
    req(input$check_permit)
    if(!input$check_permit){
      shinyjs::hide("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$selected <- NULL
    } else {
      shinyjs::show("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$selected <- input$permit
    }
  })

  permit_rv <- reactiveValues(selected = "")
  observeEvent(input$permit, {
    permit_rv$selected <- input$permit
  })

  site_rv <- reactiveValues(selected = "")
  observe({
    site_rv$selected <- translate_sites(input$site, lookup(), input$site_type)
  })

  observe({
    site_rv$selected <- translate_sites(input$site_modal, lookup(), input$site_type)
  })

  wsgroup_rv <- reactiveValues(selected = "")
  observeEvent(input$wsgroup, {
    wsgroup_rv$selected <- input$wsgroup
  })

  parameter_rv <- reactiveValues(selected = "")
  observe({
    parameter_rv$selected <- translate_parameters(input$parameter, lookup(),
                                                  input$parameter_type)
  })

  lookup <- reactive({
    run_mode_lookup(dataset())
  })

  lookup_location <- reactive({
    run_mode_lookup_location(dataset())
  })

  get_permits <- reactive({
    req(input$check_permit)
    permits(lookup())
  })

  get_sites <- reactive({
    lookup <- lookup()
    x <- permit_sites(permit_rv$selected, lookup)
    translate_sites(x, lookup, input$site_type)
  })

  get_site_locations <- reactive({
    lookup <- lookup_location()
    lookup[lookup$EMS_ID %in% get_sites(),]
  })

  get_parameters <- reactive({
    lookup <- lookup()
    x <- site_parameters(input$site, lookup)
    translate_parameters(x, lookup, input$parameter_type)
  })

  get_data <- reactive({
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    get_run_mode_data(input$parameter, input$site,
                      input$date_range[1], input$date_range[2],
                      dataset())
  })

  output$ui_site_modal <- renderUI({
    select_input_x(ns("site_modal"),
                   label = "Selected Site(s)",
                   choices = site_rv$selected,
                   selected = site_rv$selected)
  })

  output$ui_wsgroup <- renderUI({
    selectInput(ns("wsgroup"), label = "Zoom to watershed group",
                choices = c(sort(watershed_groups$WATERSHED_GROUP_NAME), ""),
                selected = wsgroup_rv$selected)
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"), label = "Permit number:",
                   choices = c(get_permits(), ""),
                   selected = "")
  })

  output$ui_site <- renderUI({
    select_input_x(ns("site"), label = NULL,
                   choices = c(get_sites(), ""),
                   selected = site_rv$selected)
  })

  output$ui_parameter <- renderUI({
    select_input_x(ns("parameter"),
                   label = NULL,
                   choices = c(get_parameters(), ""),
                   selected = parameter_rv$selected)
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
    site_rv$selected <- c(site_rv$selected,
                          translate_sites(input$site_map_marker_click$id,
                                          lookup(), input$site_type))
    updateSelectizeInput(session, ns("site_modal"), selected = site_rv$selected)
  })

  observeEvent(input$site_map_shape_click, {
    ws <- input$site_map_shape_click$id
    wsgroup_rv$selected <- ws
  })

  observeEvent(input$done, {
    removeModal()
  })

  return(get_data)
}

