# Module UI

#' @title   mod_outlier_ui and mod_outlier_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_outlier
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_outlier_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   numericInput(ns("sds"), label = "Number of standard deviations",
                                value = 10),
                   checkboxInput(ns("ignore_undetected"), "Ignore undetected", TRUE),
                   checkboxInput(ns("large_only"), "Large values only", TRUE),
                   checkboxInput(ns("delete_outliers"), "Remove outliers from plot", FALSE)),
      mainPanel(
        uiOutput(ns("ui_plot")),
        shinyjs::hidden(button(ns("clear_outliers"),
                               label = "Undo outlier selection",
                               icon = icon(NULL))))
    )
  )
}

# Module Server

#' @rdname mod_outlier
#' @export
#' @keywords internal

mod_outlier_server <- function(input, output, session, params, stand_data){
  ns <- session$ns

  outlier_data <- reactive({
    req(stand_data())
    # if(nrow(stand_data()) < 1) return()
    req(params$by())
    waiter::show_butler()
    max_cv <- maxcv(params$max_cv())
    x <- ems_outlier(
      x = stand_data(),
      by = params$by(),
      max_cv = max_cv,
      remove_blanks = params$remove_blanks(),
      FUN = params$fun(),
      sds = input$sds,
      ignore_undetected = input$ignore_undetected,
      large_only = input$large_only)
    waiter::hide_butler()
    x
  })

  outlier_rv <- reactiveValues(data = NULL)
  observe({
    outlier_rv$data <- outlier_data()
  })

  # observeEvent(input$table_clean_rows_selected, {
  #   clean_rv$data <- add_outlier_table(clean_rv$data, input$table_clean_rows_selected)
  # })

  observeEvent(input$plot_brush, {
    outlier_rv$data <- add_outlier_brush(outlier_rv$data, input$plot_brush)
  })

  outlier_data2 <- reactive({
    if(input$delete_outliers){
      return(outlier_rv$data[!outlier_rv$data$Outlier,])
    }
    outlier_rv$data
  })

  observe({
    req(outlier_rv$data)
    if(all(outlier_rv$data$Outlier == outlier_data()$Outlier))
      return(shinyjs::hide("clear_outliers"))
    shinyjs::show("clear_outliers")
  })

  observeEvent(input$clear_outliers, {
    outlier_rv$data <- outlier_data()
  })

  output$ui_plot <- renderUI({
    req(outlier_data2())
    if(nrow(outlier_data2()) < 1) return()
    tagList(
      help_text("Click and drag mouse over plot to manually select outliers.
                                             Table in 'Clean Data' tab will be automatically updated."),
      plotOutput(ns("plot_clean"), brush = brushOpts(
        id = ns("plot_brush"),
        delay = 5000
      ))
    )
  })

  output$plot_clean <- renderPlot({
    waiter::show_butler()
    p <- wqbc::plot_timeseries(outlier_data2())
    waiter::hide_butler()
    p
  })

  return(outlier_data2)
}

## To be copied in the UI
# mod_outlier_ui("outlier_ui_1")

## To be copied in the server
# callModule(mod_outlier_server, "outlier_ui_1")

