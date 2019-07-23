# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

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
mod_ems_ui <- function(id){
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
                                   plotOutput(ns('plotEms'))),
                          tabPanel("Table",
                                   br(),
                                   emsTableOutput(ns('tableEms'))),
                          tabPanel("Site Map",
                                   br(),
                                   htmlOutput(ns("htmlSiteMap")),
                                   leaflet::leafletOutput(ns("leafletSites")),
                                   plotOutput(ns("leafletSitesUpdate")))
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

  get_parameter_data <- reactive({
    req(input$selectParameter)
    get_run_mode_data(input$selectParameter, run_mode)
    })

  get_data <- reactive({
    req(input$selectSite)
    data <- get_parameter_data()
    emsid <- site_to_emsid(data, input$selectSite)
    data <- filter_2yr_data(x = data,
                    emsid = emsid,
                    from_date = input$dateRange[1],
                    to_date = input$dateRange[2])
    data
  })

  get_sites <- reactive({
    parameter_to_site(get_parameter_data())
  })

  get_locations <- reactive({
    parameter_to_location(get_parameter_data())
  })

  ########## ---------- render UI ---------- ##########
  output$uiSites <- renderUI({
    selectInputX(ns("selectSite"),
                 choices = c(get_sites(), ""), selected = "")
  })

  output$uiParameter <- renderUI({
    selectInput(ns("selectParameter"),
                label = "Select parameter:",
                choices = c(run_mode_parameter(run_mode), ""),
                selected = "")
  })

  output$uiDateRange <- renderUI({
    req(input$selectParameter)
    data <- get_parameter_data()
    dates <- parameter_to_date(data)
    dateRangeInput(ns("dateRange"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })

  output$htmlSiteMap <- renderUI({
    req(input$selectParameter)
    html_site_map(input$selectParameter)
  })

  ########## ---------- render Outputs ---------- ##########
  output$tableEms <- renderDataTable({
    get_data()
  })

  output$plotEms <- renderPlot({
    req(input$selectSite)
    data <- get_data()
    ems_plot(data = data, parameter = input$selectParameter)
  })

  marker_deselect <- ems_marker("blue")
  output$leafletSites <- leaflet::renderLeaflet({
    ems_leaflet(data = get_locations(), icon = marker_deselect)
  })

  marker_select <- ems_marker("red")
  output$leafletSitesUpdate <- renderPlot({
    data <- get_locations()[which(get_locations()$MONITORING_LOCATION %in% input$selectSite),]
    if(nrow(data) == 0) return()
    data$LeafLabel <- leaflet_labels(data)
    ems_leaflet_update(data = data, icon = marker_select)
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

