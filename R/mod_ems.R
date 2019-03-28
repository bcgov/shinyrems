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
                 selectInputX(ns("selectSite"),
                              label = "Get any available data from sites:",
                              choices = ems_site_lookup$MONITORING_LOCATION,
                             selected = ems_site_lookup$MONITORING_LOCATION[1]),
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
                                   wellPanel(dataTableOutput(ns('tableEms')), class = "wellpanel")
                                   )))
    )
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

  output$tableEms <- renderDataTable({get_data()})
  output$plotEms <- renderPlot({
    plot_ems(get_data())
  })

}

