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
mod_ems_ui <- function(id, min_date = min_db_date(), max_date = max_db_date()){
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(width = 4, class = 'sidebar',
                 selectInput(ns("selectParameter"),
                              label = "Select parameter:",
                              choices = unique(rems::ems_parameters$PARAMETER),
                              selected = unique(rems::ems_parameters$PARAMETER)[4]),
                 uiOutput(ns("uiSites")),
                 dateRangeInput(ns("dateRange"),
                                label = "Get any available data between dates:",
                                start = min_date, end = max_date,
                                min = min_date, max = max_date),
                 br(),
                 downloadButton(ns('downloadDataEms'), class = 'small-dl',
                                label = "Download Data (csv)"),
                 downloadButton(ns('downloadPlotEms'), class = 'small-dl',
                                label = "Download Plot (png)")),
    mainPanel(width = 8,
              tabsetPanel(selected = "Plot",
                          tabPanel("Plot",
                                   br(),
                                   plotOutput(ns('plotEms'))),
                          tabPanel("Table",
                                   br(),
                                   emsTableOutput(ns('tableEms'))
                                   ),
                          tabPanel("Site Map",
                                   br(),
                                   leaflet::leafletOutput(ns("leafletSites"))),
                          tabPanel("Site Table",
                                   emsTableOutput(ns("tableSites"))))
    ))
}

# Module Server

#' @rdname mod_ems
#' @export
#' @keywords internal

mod_ems_server <- function(input, output, session){
  ns <- session$ns

  get_data <- reactive({
    ems <- site_to_emsid(input$selectSite)
    param <- parameter_to_paramcode(input$selectParameter)
    dates <- input$dateRange
    filter_historic_db(emsid = ems, param_code = param, from_date = dates[1], to_date = dates[2])
  })

  get_sites <- reactive({
    parameter_to_sites(input$selectParameter)
  })

  get_locations <- reactive({
    parameter_to_location(input$selectParameter)
  })

  output$tableEms <- renderDataTable({get_data()})
  output$plotEms <- renderPlot({
    ems_plot(data = get_data())
  })

  output$uiSites <- renderUI({
    sites <- get_sites()
    selectInputX(ns("selectSite"),
                 label = "Select sites:",
                 choices = sites,
                 selected = sites[1])
  })

  output$leafletSites <- leaflet::renderLeaflet({
    ems_leaflet(data = get_locations())
  })

  output$tableSites <- renderDataTable({get_locations()})
}

