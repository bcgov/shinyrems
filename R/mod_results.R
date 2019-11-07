# Module UI

#' @title   mod_results_ui and mod_results_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_results
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   ),
      mainPanel(tabsetPanel(selected = "Plot",
                            id = ns("tabset_data"),
                            # tabPanel(title = "Clean Data",
                            #          uiOutput(ns("ui_table_clean"))),
                            tabPanel(title = "Plot",
                                     br(),
                                     radioButtons(ns("plot_type"), label = "Plot type",
                                                  choices = c("scatter", "timeseries", "boxplot"),
                                                  selected = "scatter", inline = TRUE),
                                     plotOutput(ns("plot"))),
                            tabPanel(title = "Summary Table",
                                     br(),
                                     uiOutput(ns("ui_table_summary"))
                                     )
      ))
    )
  )
}

# Module Server

#' @rdname mod_results
#' @export
#' @keywords internal

mod_results_server <- function(input, output, session, data){
  ns <- session$ns

  output$plot <- renderPlot({
    switch(input$plot_type,
           "scatter" = ems_scatter_plot(data()),
           "timeseries" = ems_timeseries_plot(data()),
           "boxplot" = ems_boxplot(data()),
           NULL)
  })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

