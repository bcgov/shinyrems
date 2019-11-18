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
                          radioButtons(ns("param_strict"), label = NULL,
                                       choices = c("in ANY of selected sites",
                                                   "in ALL of selected sites"),
                                       selected = "in ANY of selected sites"),
                          uiOutput(ns("ui_parameter")),
                          uiOutput(ns("ui_date")),
                          uiOutput(ns("ui_get")),
                          br(),
                          uiOutput(ns("ui_sample_state")),
                          uiOutput(ns("ui_sample_class")),
                          uiOutput(ns("ui_mdl_action")),
                          br(),
                          dl_button(ns("dl_raw"), "Download Raw Data"),
                          br2(),
                          dl_button(ns("dl_tidy"), "Download Tidy Data"))),
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
                          button(ns('dl_template'), label = "Download Template")))),
    mainPanel(
      tabsetPanel(selected = "Tidy Data",
                  id = ns("tabset_data"),
                  tabPanel(title = "Raw Data",
                           uiOutput(ns("ui_table_raw"))),
                  tabPanel(title = "Tidy Data",
                           uiOutput(ns("ui_table_tidy"))),
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
  observeEvent(input$dataset, {
    raw_rv$data <- empty_raw
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
    withProgress({
      check <- check_data_which(dataset)
    },
    value = 0.5,
    message = "checking for data updates ...")
    if(check[1] != "done"){
      showModal(data_download_modal(check[1], check[2], ns))
      return()
    }
    show("div_data_find")
  })

  output$ui_get <- renderUI({
    req(input$site)
    req(input$parameter)
    button(ns("get"), "Get/Update Data")
  })

  raw_rv <- reactiveValues(data = empty_raw)

  observeEvent(input$get, {
    waiter::show_butler()
    emsid <- translate_site(input$site, lookup(), input$site_type)
    raw_rv$data <- ems_data(dataset = input$dataset,
                            parameter = input$parameter,
                            emsid = emsid,
                            from_date = input$date_range[1],
                            to_date = input$date_range[2])
    waiter::hide_butler()
  })

  observe({
    if(input$dataset == "upload"){
      req(input$upload_data)
      check <- check_data_upload(input$upload_data, template())
      if(is.character(check)){
        return(showModal(error_modal(check)))
      }
      raw_rv$data <- check
    }
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
      shinyjs::html(id = "download_text", html = HTML(paste(m$message, "<br>")),
                    add = TRUE)
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
    site_parameters(input$site,
                    lookup(),
                    input$site_type,
                    input$param_strict)
  })

  template <- reactive({
    req(input$data_type)
    type <- input$data_type
    if(type == "tidy")
      return(template_tidy)
    tidy_names_to_raw(template_tidy)
  })

  tidy_data <- reactive({
    req(raw_rv$data)
    include_depth <- TRUE
    if(all_depth_na(raw_rv$data)){
      include_depth <- FALSE
    }
    ems_tidy(raw_rv$data, input$mdl_action,
             input$data_type, input$dataset,
             include_depth)
  })

  filter_data <- reactive({
    if(nrow(tidy_data()) < 1) return(empty_tidy)
    tidy_data() %>%
      dplyr::filter(SAMPLE_STATE %in% input$sample_state) %>%
      dplyr::filter(SAMPLE_CLASS %in% input$sample_class)
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
    dates <- date_range(input$site, input$parameter,
                        lookup(), input$site_type)
    dateRangeInput(ns("date_range"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })

  output$ui_sample_state <- renderUI({
    if(nrow(tidy_data()) < 1) return()
    req(input$parameter)
    x <- sort(unique(tidy_data()$SAMPLE_STATE))
    select_input_x(ns("sample_state"),
                   label = "Select values of SAMPLE_STATE to include",
                   choices = x,
                   selected = x)
  })

  output$ui_sample_class <- renderUI({
    if(nrow(tidy_data()) < 1) return()
    req(input$parameter)
    x <- sort(unique(tidy_data()$SAMPLE_CLASS))
    select_input_x(ns("sample_class"),
                   label = "Select values of SAMPLE_CLASS to include",
                   choices = x,
                   selected = x)
  })

  output$ui_mdl_action <- renderUI({
    if(nrow(tidy_data()) < 1) return()
    req(input$parameter)
    selectInput(ns("mdl_action"), label = "MDL Action",
                choices = c("zero", "mdl", "half", "na", "none"),
                selected = "zero") %>%
      embed_help("info_mdl", ns, info$mdl_action)

  })

  observeEvent(input$info_mdl, {
    shinyjs::toggle("div_info_mdl", anim = TRUE)
  })

  observeEvent(input$search_map, {
    updateTabsetPanel(session, "tabset_data", selected = "Site Map")
  })

  output$leaf <- leaflet::renderLeaflet({
    ems_leaflet(watershed_groups, get_site_locations(), input$site_type)
  })

  observe({
    req(input$tabset_data == "Site Map")
    sites <- get_site_locations()
    id <- site_col(input$site_type)

    if(is.null(input$site))
      return(
        leafletProxy('leaf') %>%
          leaflet::removeShape("Sites") %>%
          addAwesomeMarkers(data = sites,
                            icon = icon_blue,
                            lng = ~LONGITUDE,
                            lat = ~LATITUDE,
                            group = "Sites",
                            layerId = sites[[id]],
                            label = sites[[id]])
      )

    lookup <- lookup_location()
    selected <- lookup[lookup[[id]] %in% input$site,]

    leafletProxy('leaf') %>%
      leaflet::removeShape("Sites") %>%
      addAwesomeMarkers(data = sites,
                        icon = icon_blue,
                        lng = ~LONGITUDE,
                        lat = ~LATITUDE,
                        group = "Sites",
                        layerId = sites[[id]],
                        label = sites[[id]]) %>%
      addAwesomeMarkers(data = selected,
                        icon = icon_red,
                        lng = ~LONGITUDE,
                        lat = ~LATITUDE,
                        group = "Sites",
                        layerId = selected[[id]],
                        label = selected[[id]])
  })

  observe({
    req(input$wsgroup)
    zoom_to("leaf", input$wsgroup)
  })

  observeEvent(input$leaf_marker_click, {
    sites <- c(input$leaf_marker_click$id, input$site)
    updateSelectizeInput(session, "site", selected = sites)
  })

  observeEvent(input$leaf_shape_click, {
    ws <- input$leaf_shape_click$id
    updateSelectInput(session, "wsgroup", selected = ws)
  })

  output$ui_table_raw <- renderUI({
    ems_table_output(ns('table_raw'))
  })

  output$ui_table_tidy <- renderUI({
    ems_table_output(ns('table_tidy'))
  })

  output$table_raw <- DT::renderDT({
    ems_data_table(raw_rv$data)
  })

  output$table_tidy <- DT::renderDT({
    ems_data_table(filter_data())
  })

  output$dl_raw <- downloadHandler(
    filename = function() "ems_raw_data.csv",
    content = function(file) {
      readr::write_csv(raw_rv$data, file)
    })

  output$dl_tidy <- downloadHandler(
    filename = function() "ems_tidy_data.csv",
    content = function(file) {
      readr::write_csv(tidy_data(), file)
    })

  output$dl_template <- downloadHandler(
    filename = function() "ems_template.csv",
    content = function(file) {
      readr::write_csv(template_to_df(template()), file)
    }
  )

  return(filter_data)
}

