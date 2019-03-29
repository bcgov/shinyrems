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
                                   tabsetPanel(
                                     tabPanel("Map",
                                              br(),
                                              htmlOutput(ns("htmlSiteMap")),
                                              shinycssloaders::withSpinner(leaflet::leafletOutput(ns("leafletSites")))),
                                     tabPanel("Table",
                                              br(),
                                              htmlOutput(ns("htmlSiteTable")),
                                              wellPanel(DT::dataTableOutput(ns("tableSites")), class = "wellpanel"))
                                   ))
    )))
}

# Module Server

#' @rdname mod_ems
#' @export
#' @keywords internal

mod_ems_server <- function(input, output, session){
  ns <- session$ns

  ########## ---------- reactives ---------- ##########
  ems_data <- reactive({
    rems::get_ems_data(ask = FALSE)
  })

  get_data <- reactive({
    build_data(data = ems_data(),
               emsid = site_to_emsid(input$selectSite),
               param_code = parameter_to_paramcode(input$selectParameter),
               dates = input$dateRange)
  })

  get_sites <- reactive({
    parameter_to_sites(input$selectParameter)
  })

  get_locations <- reactive({
    parameter_to_location(input$selectParameter)
  })

  get_pindex <- reactive({
    p_index(get_locations(), sites$sites)
  })

  ########## ---------- render UI ---------- ##########
  output$uiSites <- renderUI({
    selectInputX(ns("selectSite"),
                 choices = get_sites())
  })

  output$htmlSiteMap <- renderUI({
    html_site_map(input$selectParameter)
  })

  output$htmlSiteTable <- renderUI({
    html_site_table(input$selectParameter)
  })

  ########## ---------- update site selection ---------- ##########
  sites <- reactiveValues(sites = list())

  observeEvent(input$leafletSites_marker_click, {
    click <- input$leafletSites_marker_click
    sites$sites <- c(sites$sites, click$id)
  })

  observeEvent(input$tableSites_rows_selected, {
    click <- input$tableSites_rows_selected
    sites$sites <- get_locations()$MONITORING_LOCATION[click]
  })

  marker_selected = leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


#   # to keep track of previously selected row
#   prev_row <- reactiveVal()
#
#   # new icon style
#   my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
#
#   observeEvent(input$tableSite_rows_selected, {
#     row_selected = qSub()[input$table01_rows_selected,]
#     proxy <- leafletProxy('map01')
#     print(row_selected)
#     proxy %>%
#       addAwesomeMarkers(popup=as.character(row_selected$mag),
#                         layerId = as.character(row_selected$id),
#                         lng=row_selected$long,
#                         lat=row_selected$lat,
#                         icon = my_icon)
#
#     # Reset previously selected marker
#     if(!is.null(prev_row()))
#     {
#       proxy %>%
#         addMarkers(popup=as.character(prev_row()$mag),
#                    layerId = as.character(prev_row()$id),
#                    lng=prev_row()$long,
#                    lat=prev_row()$lat)
#     }
#     # set new value to reactiveVal
#     prev_row(row_selected)
#   })
#
#   # map
#   output$map01 <- renderLeaflet({
#     pal <- colorNumeric("YlOrRd", domain=c(min(quakes$mag), max(quakes$mag)))
#     qMap <- leaflet(data = qSub()) %>%
#       addTiles() %>%
#       addMarkers(popup=~as.character(mag), layerId = as.character(qSub()$id)) %>%
#       addLegend("bottomright", pal = pal, values = ~mag,
#                 title = "Earthquake Magnitude",
#                 opacity = 1)
#     qMap
#   })
#
#   observeEvent(input$map01_marker_click, {
#     clickId <- input$map01_marker_click$id
#     dataTableProxy("table01") %>%
#       selectRows(which(qSub()$id == clickId)) %>%
#       selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length + 1)
#   })
# }

  observe({
    sites$sites <- input$selectSite
    })

  # observe({print(sites$sites)})
  # observe({print(get_pindex())})
  # observe({print(param_index(sites$sites))})

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

  output$leafletSites <- leaflet::renderLeaflet({
    ems_leaflet(data = get_locations())
  })

  # output$tableSites <- DT::renderDataTable({
  #   DT::datatable(get_locations(), selection = "single", options = list(stateSave = TRUE))
  # })

  output$tableSites <- DT::renderDataTable({
    DT::datatable(
      get_locations(),
      selection = list(mode = 'multiple',
                       selected = get_pindex())
    )
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

