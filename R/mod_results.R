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
                   tabsetPanel(
                     tabPanel(title = "Plot controls",
                              br(),
                              uiOutput(ns("ui_date_range")),
                              radioButtons(ns("plot_type"), label = "Plot type",
                                           choices = c("scatter", "timeseries", "boxplot"),
                                           selected = "scatter", inline = TRUE)),
                     tabPanel(title = "Rename Sites",
                              br(),
                              uiOutput(ns("ui_rename")),
                              button(ns("finalise"), label = "Rename"))
                   ),
                   br2(),
                   dl_button(ns("dl_table"), "Download Summary Table"),
                   br2(),
                   dl_button(ns("dl_plot"), "Download Plots")),
      mainPanel(tabsetPanel(selected = "Plot",
                            tabPanel(title = "Plot",
                                     br(),
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

  plots <- reactive({
    req(input$date_range)
    ems_plots(clean_rv$data, input$plot_type, input$date_range)
  })

  summary_table <- reactive({
    ems_summary_table(clean_rv$data)
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

  output$ui_plot <- renderUI({
    lapply(seq_along(plots()), plot_outputs)
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

  output$ui_date_range <- renderUI({
    if(nrow(clean_data()) < 1) return()
    date_range <- range(clean_data()$Date, na.rm = TRUE)
    tagList(
      tags$label("Adjust plot start and end date"),
      help_text("This only changes the plot x-axis, not the summary statistics."),
      dateRangeInput(ns("date_range"), label = NULL,
                     start = date_range[1], end = date_range[2])
    )
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

  clean_rv <- reactiveValues(data = NULL)
  observe({
    data <- clean_data()
    data$Site_Renamed <- data$EMS_ID
    clean_rv$data <- data
  })

  observeEvent(input$finalise, {
    sites <- unique(clean_data()$EMS_ID)
    data <- clean_data()
    for(i in sites){
      x <- input[[i]]
      data$Site_Renamed[data$EMS_ID == i] <- x
    }
    clean_rv$data <- data
  })

  rename <- function(site, ns){
    tagList(
      textInput(ns(site), label = paste("rename", site, "to"))
    )
  }

  output$ui_rename <- renderUI({
    sites <- unique(clean_data()$EMS_ID)
    lapply(sites, rename, ns)
  })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

