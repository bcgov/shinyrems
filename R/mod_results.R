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
                   tags$label("Adjust plot start and end date"),
                   help_text("This only changes the plot x-axis, not the summary statistics."),
                   uiOutput(ns("ui_date_range")),
                   dl_button(ns("dl_table"), "Download Summary Table"),
                   br2(),
                   dl_button(ns("dl_plot"), "Download Plots"),
                   uiOutput(ns("ui_dl_plot"))),
      mainPanel(tabsetPanel(selected = "Plot",
                            tabPanel(title = "Plot",
                                     br(),
                                     radioButtons(ns("plot_type"), label = "Plot type",
                                                  choices = c("scatter", "timeseries", "boxplot"),
                                                  selected = "scatter", inline = TRUE),
                                     uiOutput(ns("ui_plot"))),
                            tabPanel(title = "Summary Table",
                                     br(),
                                     tableOutput(ns("table"))
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
    req(input$date_range)
    ems_plots(clean_data(), input$plot_type, input$date_range)
  })

  summary_table <- reactive({
    ems_summary_table(clean_data())
  })

  plot_outputs <- function(x){
    tagList(
      plotOutput(ns(paste0("plot_", x))),
      br()
    )
  }

  output$table <- renderTable({
    summary_table()
  })

  # table_outputs <- function(x){
  #   tagList(
  #     tagList(
  #       tableOutput(ns(paste0("table_", x))),
  #       br()
  #     )
  #   )
  # }

  output$ui_plot <- renderUI({
    lapply(seq_along(plots()), plot_outputs)
  })

  # output$ui_table <- renderUI({
  #   lapply(seq_along(tables()), table_outputs)
  # })

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

  # observe({
  #   for (i in seq_along(tables())) {
  #     local({
  #       my_i <- i
  #       tablename <- paste0("table_", my_i)
  #       output[[tablename]] <- renderTable({
  #         tables()[my_i]
  #       })
  #     })
  #   }
  # })

  output$ui_date_range <- renderUI({
    date_range <- range(clean_data()$Date, na.rm = TRUE)
      dateRangeInput(ns("date_range"), label = NULL,
                     start = date_range[1], end = date_range[2])

  })

  output$dl_plot <- downloadHandler(
    filename = function(){
      "ems_plots.zip"
    },
    content = function(file){
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in seq_along(plots())) {
        path <- paste0("plot_", i, ".png")
        fs <- c(fs, path)
        ggplot2::ggsave(path, plots()[[i]], device = "png")
      }
      zip(zipfile = file, files = fs)
    },
    contentType = "application/zip"
  )

  output$dl_table <- downloadHandler(
    filename = function() "ems_summary_table.csv",
    content = function(file) {
      readr::write_csv(summary_table(), file)
    })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

