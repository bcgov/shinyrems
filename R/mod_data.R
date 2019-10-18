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

  sidebarLayout(
    sidebarPanel(
      radioButtons(ns("dataset"), label = "Select dataset",
                   choices = datasets,
                   selected = "demo", inline = TRUE),
      shinyjs::hidden(div(id = ns("div_data_find"),
                          tags$label("Select site(s) or"),
                          actionLink(ns("search_map"), label = "find sites on map"),
                          checkboxInput(ns("check_permit"),
                                        label = "Filter by Permit Number",
                                        value = FALSE),
                          uiOutput(ns("ui_permit")),
                          radioButtons(ns("site_type"), label = NULL,
                                       choices = c("Monitoring Location", "EMS ID"),
                                       selected = "Monitoring Location", inline = TRUE),
                          uiOutput(ns("ui_site")),
                          tags$label("Select Parameter(s)"),
                          radioButtons(ns("param_type"), label = NULL,
                                       choices = c("Parameter Name", "Parameter Code"),
                                       selected = "Parameter Name", inline = TRUE),
                          uiOutput(ns("ui_parameter")),
                          uiOutput(ns("ui_date")))),
      shinyjs::hidden(div(id = ns("div_data_upload"),
                          radioButtons(ns("data_type"), label = "Data format",
                                       choices = c("Tidied EMS Data" = "tidy",
                                                   "Raw EMS Data" = "raw"),
                                       selected = "tidy"),
                          fileInput(ns("upload_data"),
                                    buttonLabel = span(tagList(icon("upload"), "csv")),
                                    label = "",
                                    placeholder = "Upload your own dataset",
                                    accept = c('.csv')),
                          button(ns('dl_template'), label = "Download Template"),
                          downloadButton(ns("dl_template_handler"), label = NULL,
                                         style = "visibility: hidden;"))),
      uiOutput(ns("ui_download"))
    ),
    mainPanel(
      tabsetPanel(selected = "Data",
                  id = ns("tabset_data"),
                  tabPanel(title = "Data",
                           uiOutput(ns("view_table")),
                           downloadButton(ns("dl_data_handler"), label = NULL,
                                          style = "visibility: hidden;")),
                  tabPanel(title = "Site Map",
                           wellPanel(site_map(ns), class = "wellpanel"))
      )
    )
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
    hide("div_data_upload")
    showTab("tabset_data", target = "Site Map", session = session)
    updateTabsetPanel(session, "tabset_data", selected = "Data")
    dataset <- input$dataset
    if(dataset == "upload"){
      return({
        show("div_data_upload")
        hideTab("tabset_data", target = "Site Map", session = session)
        })
    }
    if(dataset == "demo"){
      return(show("div_data_find"))
    }
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
    permit_sites(input$permit, lookup(), input$site_type)
  })

  get_site_locations <- reactive({
    lookup <- lookup_location()
    x <- site_col(input$site_type)
    lookup[lookup[[x]] %in% get_sites(),]
  })

  get_parameters <- reactive({
    site_parameters(input$site, lookup(), input$site_type, input$param_type)
  })

  get_data <- reactive({
    if(input$dataset == "upload"){
      req(input$upload_data)
      return(check_data_upload(input$upload_data, template()))
    }
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    ems_data_progress(input$dataset, input$parameter, input$site,
                      input$date_range[1], input$date_range[2],
                      input$site_type, input$param_type, lookup())
  })

  output$ui_map_site <- renderUI({
    select_input_x(ns("map_site"),
                   label = "Selected Site(s)",
                   choices = input$site,
                   selected = input$site,
                   width = 400)
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
    dates <- date_range(input$site, input$parameter, lookup(),
                        input$site_type, input$param_type)
    dateRangeInput(ns("date_range"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })

  observeEvent(input$search_map, {
    updateTabsetPanel(session, "tabset_data", selected = "Site Map")
  })

  output$leaf <- leaflet::renderLeaflet({
    ems_leaflet(watershed_groups, get_site_locations(), input$site_type)
  })

  observe({
    req(input$wsgroup)
    zoom_to("leaf", input$wsgroup)
  })

  observeEvent(input$leaf_marker_click, {
    sites <- c(input$leaf_marker_click$id, input$site)
    updateSelectizeInput(session, "map_site", selected = sites)
    updateSelectizeInput(session, "site", selected = sites)
  })

  observeEvent(input$map_site, {
    updateSelectizeInput(session, "site", selected = input$map_site)
  })

  observeEvent(input$leaf_shape_click, {
    ws <- input$leaf_shape_click$id
    updateSelectInput(session, "wsgroup", selected = ws)
  })

  output$ui_download <- renderUI({
    if(is.null(get_data()) || input$dataset == "upload")
      return()
    button(ns('dl_data'), label = "Download Raw Data")
  })

  output$view_table <- renderUI({
    req(get_data())
    if(is.character(get_data())){
      return(showModal(error_modal(get_data())))
    }
    ems_table_output(ns('data_table'))
  })

  output$data_table <- DT::renderDT({
    ems_data_table(get_data())
  })

  observeEvent(input$dl_data, {
    shinyjs::runjs(click_js(ns("dl_data_handler")))
  })

  output$dl_data_handler <- downloadHandler(
    filename = function() "ems_data.csv",
    content = function(file) {
      readr::write_csv(get_data(), file)
    })

  observeEvent(input$dl_template, {
    shinyjs::runjs(click_js(ns("dl_template_handler")))
  })

  template <- reactive({
    type <- input$data_type
    if(type == "tidy")
      return(template_tidy)
    template_raw
  })

  output$dl_template_handler <- downloadHandler(
    filename = function() "ems_template.csv",
    content = function(file) {
      readr::write_csv(template_to_df(template()), file)
    })

  return(get_data)
}

