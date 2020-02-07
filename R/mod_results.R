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
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   uiOutput(ns("ui_date_range")),
                   uiOutput(ns("ui_type")),
                   fillRow(height = 75,
                           uiOutput(ns("ui_facet")),
                           uiOutput(ns("ui_colour"))),
                   actionLink(ns("rename"), "Rename sites"),
                   br(),
                   uiOutput(ns("ui_rename")),
                   br(),
                   sliderInput(ns("plot_height"), label = "Plot Height",
                               value = 500, min = 0, max = 1000, step = 100)),
      mainPanel(tabsetPanel(selected = "Plot",
                            tabPanel(title = "Plot",
                                     br(),
                                     dl_group("plot", ns),
                                     br2(),
                                     uiOutput(ns("ui_plot"))),
                            tabPanel(title = "Summary Statistics",
                                     br(),
                                     dl_group("table", ns),
                                     br2(),
                                     tableOutput(ns("table"))
                                     ),
                            tabPanel(title = "R Code",
                                     br(),
                                     wellPanel(uiOutput(ns("rcode"))))
      ))
    )
  )
}

# Module Server

#' @rdname mod_results
#' @export
#' @keywords internal

mod_results_server <- function(input, output, session, clean){
  ns <- session$ns

  observe({
    req(input$plot_type)
    if(input$plot_type == "scatter"){
      show("div_geom")
      hide("timeframe")
    } else {
      hide("div_geom")
      show("timeframe")
    }
  })

  plots <- reactive({
    req(input$date_range)
    req(input$facet)
    req(input$colour)
    ems_plots(clean_rv$data, input$plot_type,
              input$geom, input$date_range,
              input$point_size, input$line_size,
              input$facet, input$colour, input$timeframe)
  })

  summary_table <- reactive({
    suppressWarnings(waiter::show_butler())
    x <- ems_summary_table(clean_rv$data)
    suppressWarnings(waiter::hide_butler())
    x
  })

  output$table <- renderTable({
    summary_table()
  })

  output$ui_plot <- renderUI({
    lapply(seq_along(plots()), plot_outputs, ns, input$plot_height)
  })

  observe({
    req(plots())
    suppressWarnings(waiter::show_butler())
    for (i in seq_along(plots())) {
      local({
        my_i <- i
        plotname <- paste0("plot_", my_i)
        output[[plotname]] <- renderPlot({
          plots()[my_i]
        })
      })
    }
    suppressWarnings(waiter::hide_butler())
  })

  output$ui_date_range <- renderUI({
    req(clean$data())
    if(nrow(clean$data()) < 1) return()
    date_range <- range(clean$data()$Date, na.rm = TRUE)
    tagList(
      tags$label("Adjust plot start and end date"),
      help_text("This only changes the plot x-axis,
                not the underlying data and summary table."),
      dateRangeInput(ns("date_range"), label = NULL,
                     start = date_range[1], end = date_range[2])
    )
  })

  output$ui_type <- renderUI({
    tagList(
      radioButtons(ns("plot_type"), label = "Plot type",
                   choices = c("scatter", "boxplot"),
                   selected = "scatter", inline = TRUE),
      shinyjs::hidden(div(id = ns("div_geom"),
        checkboxGroupInput(ns("geom"), label = NULL,
                                         choices = c("show lines", "show points"),
                                         selected = c("show points", "show lines"),
                                         inline = TRUE),
        fillRow(height = 75,
                numericInput(ns("point_size"), label = "Point size", value = 1.5,
                             min = 0.1, max = 10),
                numericInput(ns("line_size"), label = "Line size", value = 0.3,
                             min = 0.1, max = 10)
        ))),
      shinyjs::hidden(selectInput(ns("timeframe"), label = "Group by time window",
                                  choices = c("Year", "Year-Month", "Month", "Season"),
                                  selected = c("Year")) %>%
                        embed_help("info_timeframe", ns, info$timeframe))
    )
  })

  output$ui_facet <- renderUI({
    data <- clean_rv$data
    x <- sort(intersect(names(data), c("Variable", "EMS_ID")))
    selectInput(ns("facet"), "Facet by",
                choices = x,
                selected = "Variable")
  })

  output$ui_colour <- renderUI({
    data <- clean_rv$data
    x <- sort(intersect(names(data), c("Variable", "EMS_ID")))
    selectInput(ns("colour"), "Colour by",
                choices = x,
                selected = x[1])
  })

  output$dl_plot <- downloadHandler(
    filename = function(){
      paste0(input$file_plot, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plots()[[1]], device = "png")
    })

  output$dl_table <- downloadHandler(
    filename = function(){
      paste0(input$file_table, ".csv")
    },
    content = function(file) {
      readr::write_csv(summary_table(), file)
    })

  # output$dl_plot <- downloadHandler(
  #   filename = function(){
  #     "ems_plots.zip"
  #   },
  #   content = function(file){
  #     fs <- c()
  #     tmpdir <- tempdir()
  #     setwd(tempdir())
  #     for (i in seq_along(plots())) {
  #       path <- paste0("plot_", i, ".png")
  #       fs <- c(fs, path)
  #       ggplot2::ggsave(path, plots()[[i]], device = "png")
  #     }
  #     zip(zipfile = file, files = fs)
  #   },
  #   contentType = "application/zip"
  # )

  # output$dl_table <- downloadHandler(
  #   filename = function() "ems_summary_table.csv",
  #   content = function(file) {
  #     readr::write_csv(summary_table(), file)
  #   })

  clean_rv <- reactiveValues(data = NULL)
  observe({
    data <- clean$data()
    data$EMS_ID_Renamed <- data$EMS_ID
    clean_rv$data <- data
  })

  observeEvent(input$finalise, {
    data <- clean_rv$data
    sites <- unique(data$EMS_ID)
    for(i in sites){
      x <- input[[i]]
      data$EMS_ID_Renamed[data$EMS_ID == i] <- x
    }
    clean_rv$data <- data
  })

  observeEvent(input$rename, {
    shinyjs::toggle("div_rename", anim = TRUE, animType = "slide")
  })

  output$ui_rename <- renderUI({
    sites <- unique(clean_rv$data$EMS_ID)
    shinyjs::hidden(div(id = ns("div_rename"),
        lapply(sites, rename_inputs, ns),
        button(ns("finalise"), "Rename")))
  })

  observeEvent(input$info_timeframe, {
    shinyjs::toggle("div_info_timeframe", anim = TRUE)
  })

  rcodeplot <- reactive({})

  rcodetable <- reactive({})

  return(
    list(
      facet = reactive({input$facet}),
      colour = reactive({input$colour}),
      rcodeplot = rcodeplot,
      rcodetable = rcodetable
    )
  )
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

