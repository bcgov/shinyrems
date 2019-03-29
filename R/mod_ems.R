# Module UI

#' @title   mod_ems_ui and mod_ems_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ems
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_ems_ui <- function(id, dates = run_mode_date_range()){
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(width = 4, class = 'sidebar',
                 uiOutput(ns("uiParameter")),
                 uiOutput(ns("uiSites")),
                 uiOutput(ns("uiDateRange")),
                 br(),
                 emsDownload(ns('dlEmsData'), br = FALSE),
                 emsDownload(ns('dlEmsPlot'), label = "Download Plot (png)", br = FALSE)),
    mainPanel(width = 8,
              tabsetPanel(selected = "Plot",
                          tabPanel("Plot",
                                   br(),
                                   shinycssloaders::withSpinner(plotOutput(ns('plotEms')))),
                          tabPanel("Table",
                                   br(),
                                   emsTableOutput(ns('tableEms'))
                          ),
                          tabPanel("Site Selection",
                                   br(),
                                   htmlOutput(ns("htmlSiteMap")),
                                   shinycssloaders::withSpinner(leaflet::leafletOutput(ns("leafletSites"))))
              )))
}

# Module Server

#' @rdname mod_ems
#' @export
#' @keywords internal

mod_ems_server <- function(input, output, session){
  ns <- session$ns
  run_mode <- getShinyOption("run_mode", "demo")

  ########## ---------- reactives ---------- ##########
  ems_data <- reactive({
    rems::get_ems_data(ask = FALSE, dont_update = TRUE)
  })

  get_data <- reactive({
    run_mode_data(run_mode = run_mode,
                  data = ems_data(),
                  emsid = site_to_emsid(input$selectSite),
                  param_code = parameter_to_paramcode(input$selectParameter),
                  dates = input$dateRange)
  })

  get_parameter <- reactive({
    get_parameter_lookup(run_mode = run_mode)$PARAMETER %>% unique()
  })

  get_sites <- reactive({
    parameter_to_sites(input$selectParameter, run_mode = run_mode)
  })

  get_locations <- reactive({
    parameter_to_location(input$selectParameter, run_mode = run_mode)
  })

  get_pindex <- reactive({
    p_index(get_locations(), sites$sites)
  })

  ########## ---------- render UI ---------- ##########
  output$uiSites <- renderUI({
    selectInputX(ns("selectSite"),
                 choices = get_sites())
  })

  output$uiParameter <- renderUI({
    selectInput(ns("selectParameter"),
                label = "Select parameter:",
                choices = get_parameter(),
                selected = get_parameter()[1])
  })

  output$uiDateRange <- renderUI({
    dates <- run_mode_date_range(run_mode = run_mode)
    dateRangeInput(ns("dateRange"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })

  output$htmlSiteMap <- renderUI({
    html_site_map(input$selectParameter)
  })

  ########## ---------- update site selection ---------- ##########
  sites <- reactiveValues(sites = list())

  ### --- map
  # if marker clicked/unclicked adjust sites$sites
  observeEvent(input$leafletSites_marker_click, {
    click_id <- input$leafletSites_marker_click$id
    # if(click_id %in% sites$sites){
    #   return(sites$sites <- setdiff(sites$sites, click_id))
    # }
    sites$sites <- c(sites$sites, click_id)
  })

  # always adjust selected site markers when site$site changes
  # marker_select <- ems_marker("red")
  # observe({
  #   data <- get_locations()[which(get_locations()$MONITORING_LOCATION %in% sites$sites),]
  #   if(nrow(data) == 0) return()
  #   data$LeafLabel <- leaflet_labels(data)
  #   ems_leaflet_update(data = data, icon = marker_select)
  # })

  ### --- dropdown
  # adjusting site selection at dropdown updates sites$sites
  observe({
    sites$sites <- input$selectSite
  })

  # adjust dropdown selection when sites$sites changes
  observe({
    updateSelectizeInput(session, 'selectSite', selected = sites$sites)
  })

  ########## ---------- render Outputs ---------- ##########
  output$tableEms <- renderDataTable({
    get_data()
  })

  output$plotEms <- renderPlot({
    ems_plot(data = get_data(), parameter = input$selectParameter)
  })

  marker_deselect <- ems_marker("blue")
  output$leafletSites <- leaflet::renderLeaflet({
    ems_leaflet(data = get_locations(), icon = marker_deselect)
  })

  ########## ---------- download handlers ---------- ##########
  output$dlEmsData <- downloadHandler(
    filename = function() "ems_data.csv",
    content = function(file) {
      readr::write_csv(get_data(), file)
    })

  output$dlEmsPlot <- downloadHandler(
    filename = function() "ems_plot.png",
    content = function(file) {
      ggplot2::ggsave(file, plot = ems_plot(data = get_data(), parameter = input$selectParameter),
                      width = 8, height = 6, device = "png")
    })
}

