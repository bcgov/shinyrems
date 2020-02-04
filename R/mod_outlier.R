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
                   checkboxInput(ns("ignore_undetected"), "Ignore values below detection limit", TRUE)  %>%
                     embed_help("info_undetected", ns, info$undetected),
                   checkboxInput(ns("large_only"), "Large values only", TRUE)  %>%
                     embed_help("info_large", ns, info$large),
                   checkboxInput(ns("delete_outliers"), "Remove outliers from plot", FALSE) %>%
                     embed_help("info_remove", ns, info$remove),
                   numericInput(ns("point_size"), label = "Point Size",
                                value = 1.3, min = 0, max = 10)),
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Manual outlier selection",
                   br(),
                   uiOutput(ns("ui_plot")),
                   shinyjs::hidden(button(ns("clear_outliers"),
                                          label = "Undo outlier selection",
                                          icon = icon(NULL)))),
          tabPanel(title = "Final Data",
                   br(),
                   dl_group("final", ns),
                   br2(),
                   uiOutput(ns("ui_table_final"))),
          tabPanel(title = "Messages",
                   br(),
                   help_output(ns("console_clean"))),
          tabPanel(title = "R Code",
                   br(),
                   wellPanel(uiOutput(ns("rcode")))))
        )
    )
  )
}

# Module Server

#' @rdname mod_outlier
#' @export
#' @keywords internal

mod_outlier_server <- function(input, output, session, clean, stand){
  ns <- session$ns

  clean_data <- reactive({
    suppressWarnings(waiter::show_butler())
    withCallingHandlers({
      shinyjs::html("console_clean", "")
      x <- ems_aggregate(stand$data(),
                         by = input$by,
                         remove_blanks = input$remove_blanks,
                         max_cv = max_cv(),
                         FUN = input$fun)},
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
    suppressWarnings(waiter::hide_butler())
    x
  })

  outlier_data <- reactive({
    req(stand$data())
    req(clean$by())
    suppressWarnings(waiter::show_butler())
    withCallingHandlers({
      shinyjs::html("console_clean", "")
      x <- ems_outlier(
        x = stand$data(),
        by = clean$by(),
        max_cv = max_cv(),
        remove_blanks = clean$remove_blanks(),
        FUN = clean$fun(),
        sds = input$sds,
        ignore_undetected = input$ignore_undetected,
        large_only = input$large_only)},
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
    suppressWarnings(waiter::hide_butler())
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

  observeEvent(input$info_undetected, {
    shinyjs::toggle("div_info_undetected", anim = TRUE)
  })

  observeEvent(input$info_large, {
    shinyjs::toggle("div_info_large", anim = TRUE)
  })

  observeEvent(input$info_remove, {
    shinyjs::toggle("div_info_remove", anim = TRUE)
  })

  rcodeclean <- reactive({
    rcode_clean2(by = clean$by(), max_cv = max_cv(), sds = input$sds,
                 ignore_undetected = input$ignore_undetected,
                 large_only = input$large_only,
                 remove_blanks = clean$remove_blanks(), fun = clean$fun())
  })

  rcodeoutlier <- reactive({
    rcode_outlier(manual_outliers())
  })

  output$rcode <- renderUI({
    tagList(
      rcodeclean(),
      br2(),
      rcodeoutlier()
    )
  })

  output$ui_table_final <- renderUI({
    ems_table_output(ns('table_final'))
  })

  output$table_final <- DT::renderDT({
    ems_data_table(outlier_data3())
  })

  output$dl_final <- downloadHandler(
    filename = function(){
      paste0(input$file_final, ".csv")
    },
    content = function(file) {
      readr::write_csv(outlier_data3(), file)
    })

  return(
    list(
      data = outlier_data3,
      rcodeclean = rcodeclean,
      rcodeoutlier = rcodeoutlier
    )
  )
}

## To be copied in the UI
# mod_outlier_ui("outlier_ui_1")

## To be copied in the server
# callModule(mod_outlier_server, "outlier_ui_1")

