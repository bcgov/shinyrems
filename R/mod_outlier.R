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
mod_outlier_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   numericInput(ns("sds"), label = "Standard deviations",
                                value = 10) %>%
                     embed_help("info_sds", ns, info$sds),
                   checkboxInput(ns("ignore_undetected"), "Ignore undetected", TRUE),
                   checkboxInput(ns("large_only"), "Large values only", TRUE),
                   checkboxInput(ns("delete_outliers"), "Remove outliers from plot", FALSE),
                   numericInput(ns("point_size"), label = "Point Size",
                                value = 1.3, min = 0, max = 10)),
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

mod_outlier_server <- function(input, output, session, clean, stand){
  ns <- session$ns

  outlier_data <- reactive({
    req(stand$data())
    req(clean$by())
    x <- ems_outlier(
      x = stand$data(),
      by = clean$by(),
      max_cv = max_cv(),
      remove_blanks = clean$remove_blanks(),
      FUN = clean$fun(),
      sds = input$sds,
      ignore_undetected = input$ignore_undetected,
      large_only = input$large_only)
    x
  })

  outlier_rv <- reactiveValues(data = NULL)
  observe({
    outlier_rv$data <- outlier_data()
  })

  # observeEvent(input$table_clean_rows_selected, {
  #   clean_rv$data <- add_outlier_table(clean_rv$data, input$table_clean_rows_selected)
  # })

  manual_outliers <- reactive({
    which(add_outlier_brush(outlier_rv$data, input$plot_brush)$Outlier)
  })

  observeEvent(input$plot_brush, {
    outlier_rv$data <- add_outlier_brush(outlier_rv$data, input$plot_brush)
  })

  outlier_data2 <- reactive({
    if(input$delete_outliers){
      return(outlier_rv$data[!outlier_rv$data$Outlier,])
    }
    outlier_rv$data
  })

  max_cv <- reactive({
    maxcv(clean$max_cv())
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
    suppressWarnings(waiter::show_butler())
    p <- plot_outlier(outlier_data2(), clean$by(), input$point_size)
    suppressWarnings(waiter::hide_butler())
    p
  })

  outlier_data3 <- reactive({
    req(outlier_data2())
    x <- outlier_data2()
    x[!x$Outlier,]
  })

  observeEvent(input$info_sds, {
    shinyjs::toggle("div_info_sds", anim = TRUE)
  })

  return(
    list(
      data = outlier_data3,
      by = reactive({clean$by()}),
      fun = reactive({clean$fun()}),
      remove_blanks = reactive({clean$remove_blanks()}),
      max_cv = max_cv,
      sds = reactive({input$sds}),
      ignore_undetected = reactive({input$ignore_undetected}),
      large_only = reactive({input$large_only}),
      delete_outliers = reactive({input$delete_outliers}),
      manual_outliers = manual_outliers
    )
  )
}

## To be copied in the UI
# mod_outlier_ui("outlier_ui_1")

## To be copied in the server
# callModule(mod_outlier_server, "outlier_ui_1")

