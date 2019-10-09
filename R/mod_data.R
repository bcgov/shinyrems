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
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4, class = 'sidebar',
                   tabsetPanel(id = ns("data_type"),
                     tabPanel(title = "Find Data",
                              value = "data",
                              checkboxInput(ns("check_permit"),
                                            label = "Filter by Permit Number",
                                            value = FALSE),
                              uiOutput(ns("ui_permit")),
                              tags$label("Select site(s) or"),
                              actionLink(ns("search_map"), label = "find sites on map"),
                              radioButtons(ns("site_type"), label = NULL,
                                           choices = c("Monitoring Location", "EMS ID"),
                                           selected = "Monitoring Location", inline = TRUE),
                              uiOutput(ns("ui_site")),
                              tags$label("Select Parameter(s)"),
                              radioButtons(ns("parameter_type"), label = NULL,
                                           choices = c("Parameter Name", "Parameter Code"),
                                           selected = "Parameter Name", inline = TRUE),
                              uiOutput(ns("ui_parameter")),
                              uiOutput(ns("ui_date"))),
                     tabPanel(title = "Upload Data",
                              value = "upload",
                              fileInput(ns("upload_data"),
                                        buttonLabel = span(tagList(icon("upload"), "csv")),
                                        label = "",
                                        placeholder = "Upload your own dataset",
                                        accept = c('.csv')),
                              button(ns('dl_template'), label = "Download Template"),
                              downloadButton(ns("dl_template_handler"), label = NULL,
                                             style = "visibility: hidden;")))),
      mainPanel(width = 8,
                # invisible download handlers so can use bootstrap buttons
                uiOutput(ns("preview_table")),
                downloadButton(ns("dl_data_handler"), label = NULL,
                               style = "visibility: hidden;"))
  ))
}

# Module Server

#' @rdname mod_data
#' @export
#' @keywords internal

mod_data_server <- function(input, output, session){
  ns <- session$ns
  run_mode <- getShinyOption("run_mode", "demo")

  ########## ---------- Find Data tab ---------- ##########
  observe({
    if(!input$check_permit){
      shinyjs::hide("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$permit <- NULL
    } else {
      shinyjs::show("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$permit <- input$permit
    }
  })

  sitemap_modal <- reactive({
    tagList(
      help_text("Click a marker to add to selected sites.
          Select from dropdown or click polygon to zoom
                to watershed group."),
      uiOutput(ns("ui_wsgroup")),
      shinycssloaders::withSpinner(leaflet::leafletOutput(ns("site_map"))),
      br(),
      uiOutput(ns("ui_site_modal"))
    )
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

  observe({
    req(input$wsgroup)
    ws <- watershed_groups[watershed_groups$WATERSHED_GROUP_NAME == input$wsgroup,]
    leafletProxy("site_map") %>%
      setView(lng = ws$lng_center, lat = ws$lat_center, zoom = 8L)
  })

  output$site_map <- leaflet::renderLeaflet({
    ems_leaflet(watershed_groups, get_site_locations(), input$site_type)
  })

  observeEvent(input$search_map, {
    showModal(modalDialog(
      sitemap_modal(),
      easyClose = TRUE,
      title = "Find sites on map",
      footer = modalButton("Done")
    ))
  })

  lookup <- reactive({
    run_mode_lookup(run_mode)
  })

  lookup_location <- reactive({
    run_mode_lookup_location(run_mode)
  })

  permit_rv <- reactiveValues(permit = "")
  observeEvent(input$permit, {
    permit_rv$permit <- input$permit
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

  observeEvent(input$site_map_marker_click, {
    site_rv$selected <- c(site_rv$selected,
                          translate_sites(input$site_map_marker_click$id,
                                          lookup(), input$site_type))
    updateSelectizeInput(session, ns("site_modal"), selected = site_rv$selected)
  })

  observeEvent(input$site_map_shape_click, {
    shp <- input$site_map_shape_click$id
    ws <- watershed_groups[watershed_groups$WATERSHED_GROUP_NAME == shp,]
    wsgroup_rv$selected <- ws$WATERSHED_GROUP_NAME
    leafletProxy("site_map") %>%
      setView(lng = ws$lng_center, lat = ws$lat_center, zoom = 8L)
  })

  parameter_rv <- reactiveValues(selected = "")
  observe({
    parameter_rv$selected <- translate_parameters(input$parameter, lookup(),
                                                  input$parameter_type)
  })

  get_permits <- reactive({
    if(!input$check_permit)
      return()
    c(permits(lookup()), "")
  })

  get_sites <- reactive({
    lookup <- lookup()
    x <- permit_sites(permit_rv$permit, lookup)
    c(translate_sites(x, lookup, input$site_type), "")
  })

  get_site_locations <- reactive({
    lookup <- lookup_location()
    sites <- setdiff(get_sites(), "")
    lookup[lookup$EMS_ID %in% sites,]
  })

  get_parameters <- reactive({
    lookup <- lookup()
    x <- site_parameters(input$site, lookup)
    c(translate_parameters(x, lookup, input$parameter_type), "")
  })

  get_data <- reactive({
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    get_run_mode_data(input$parameter, input$site,
                                     input$date_range[1], input$date_range[2],
                                     run_mode)
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"), label = "Permit number:",
                   choices = get_permits(),
                   selected = "")
  })

  output$ui_site <- renderUI({
    select_input_x(ns("site"), label = NULL,
                 choices = get_sites(),
                 selected = site_rv$selected)
  })

  output$ui_parameter <- renderUI({
    select_input_x(ns("parameter"),
                label = NULL,
                choices = get_parameters(),
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

  ########## ---------- Upload Data tab ---------- ##########
  observeEvent(input$dl_template, {
    shinyjs::runjs(click_js(ns("dl_template_handler")))
  })

  output$dl_template_handler <- downloadHandler(
    filename = function() "ems_template.csv",
    content = function(file) {
      readr::write_csv(template_to_df(template), file)
    })

  ########## ---------- Preview Data tab ---------- ##########
  preview_data <- reactive({
    if(input$data_type == "data"){
      return(get_data())
    }
    data <- input$upload_data
    req(data)
    if(!grepl(".csv", data$name, fixed = TRUE)) {
      return("Please submit a csv file.")
    }
    data <- readr::read_csv(data$datapath)
    x <- check_template(data, template)
    if(is.character(x)){
      return(x)
    }
    data
  })

  output$preview_table <- renderUI({
    data <- preview_data()
    if(is.character(data)){
      return(error_text(data))
    }
    tagList(
      button(ns('dl_data'), label = "Download Raw Data"),
      br(),
      ems_table_output(ns('data_table'))
    )
  })

  output$data_table <- DT::renderDT({
    ems_data_table(preview_data())
  })

  observeEvent(input$dl_data, {
    shinyjs::runjs(click_js(ns("dl_data_handler")))
  })

  output$dl_data_handler <- downloadHandler(
    filename = function() "ems_data.csv",
    content = function(file) {
      readr::write_csv(preview_data(), file)
    })
}


