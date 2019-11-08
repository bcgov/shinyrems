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
                   textInput(ns("plot_title"), "Plot title"),
                   dl_button(ns("dl_table"), "Download Summary Table"),
                   br2(),
                   dl_button(ns("dl_plot"), "Download Plots"),
                   uiOutput(ns("ui_dl_plot"))),
      mainPanel(tabsetPanel(selected = "Plot",
                            id = ns("tabset_data"),
                            tabPanel(title = "Plot",
                                     br(),
                                     radioButtons(ns("plot_type"), label = "Plot type",
                                                  choices = c("scatter", "timeseries", "boxplot"),
                                                  selected = "scatter", inline = TRUE),
                                     uiOutput(ns("ui_plot"))),
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

mod_results_server <- function(input, output, session, clean_data){
  ns <- session$ns

  output$dl_plot <- downloadHandler(
    filename = function() "ems_plot.png",
    content = function(file) {
      readr::write_csv(raw_data(), file)
    })

  output$dl_table <- downloadHandler(
    filename = function() "ems_summary_table.csv",
    content = function(file) {
      readr::write_csv(sumary_table(), file)
    })

  plots <- reactive({
    ems_plots(clean_data(), input$plot_type)
  })

  create_tabs <- function(x, plots){
    title <- names(plots)[x]
    tabPanel(title = title,
             br(), plotOutput(ns(paste0("plot_", x))))
  }

  create_tabs2 <- function(x){
    tagList(
      plotOutput(ns(paste0("plot_", x))),
      br()
    )
  }

  output$ui_plot <- renderUI({
    lapply(seq_along(plots()), create_tabs2)
  })

  observe({
    for (i in seq_along(plots())) {
      local({
        my_i <- i
        plotname <- paste0("plot_", my_i)
        output[[plotname]] <- renderPlot({
          plots()[my_i]
        })
      })
    }
  })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

