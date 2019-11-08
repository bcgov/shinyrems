# Module UI

#' @title   mod_refine_ui and mod_refine_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_refine
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_clean_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 h3("Clean Data"),
                 uiOutput(ns("ui_sample_state")),
                 uiOutput(ns("ui_sample_class")),
                 checkboxInput(ns("remove_blanks"), "Remove blanks", value = TRUE),
                 uiOutput(ns("ui_by")),
                 numericInput(ns("max_cv"), label = "Maximum CV", value = Inf),
                 h3("Outlier Detection"),
                 numericInput(ns("sds"), label = "Number of standard deviations",
                              value = 10),
                 checkboxInput(ns("ignore_undetected"), "Ignore undetected", TRUE),
                 checkboxInput(ns("large_only"), "Large values only", TRUE),
                 checkboxInput(ns("delete_outliers"), "Detele outliers", FALSE),
                 shinyjs::hidden(button(ns("dl_clean"), "Download Clean Data"))),
    mainPanel(tabsetPanel(selected = "Clean Data",
                          id = ns("tabset_data"),
                          tabPanel(title = "Clean Data",
                                   uiOutput(ns("ui_table_clean"))),
                          tabPanel(title = "Select Outliers",
                                   br(),
                                   help_text("Click and drag mouse over plot to manually select outliers.
                                             Table in 'Clean Data' tab will be automatically updated."),
                                   plotOutput(ns("plot_clean"), brush = brushOpts(
                                     id = ns("plot_brush"),
                                     delay = 5000
                                   )),
                                   shinyjs::hidden(button(ns("clear_outliers"),
                                                          label = "Clear selected outliers",
                                                          icon = icon(NULL)))),
                          tabPanel(title = "Messages",
                                   br(),
                                   help_output(ns("console_clean")))
    ))
  )
}

# Module Server

#' @rdname mod_refine
#' @export
#' @keywords internal

mod_clean_server <- function(input, output, session, stand_data){
  ns <- session$ns

  observe({
    show_hide(clean_data(), "dl_clean")
  })

  output$ui_sample_state <- renderUI({
    req(stand_data())
    x <- sort(unique(stand_data()$SAMPLE_STATE))
    select_input_x(ns("sample_state"),
                   label = "Select values of SAMPLE_STATE to include",
                   choices = x,
                   selected = x)
  })

  output$ui_sample_class <- renderUI({
    req(stand_data())
    x <- sort(unique(stand_data()$SAMPLE_CLASS))
    select_input_x(ns("sample_class"),
                   label = "Select values of SAMPLE_CLASS to include",
                   choices = x,
                   selected = x)
  })

  output$ui_by <- renderUI({
    req(stand_data())
    selected <- intersect(names(stand_data()),
                     c("EMS_ID", "UPPER_DEPTH", "LOWER_DEPTH"))
    optional <- intersect(names(stand_data()),
                          c("SAMPLE_STATE", "SAMPLE_CLASS"))

    select_input_x(ns("by"), label = "Summarise data by",
                   choices = c(selected, optional),
                   selected = selected)
  })

  clean_data <- reactive({
    req(stand_data())
    data <- stand_data()
    max_cv <- maxcv(input$max_cv)
    data <- stand_data() %>%
      dplyr::filter(SAMPLE_STATE %in% input$sample_state) %>%
      dplyr::filter(SAMPLE_CLASS %in% input$sample_class)

    withCallingHandlers({
      shinyjs::html("console_clean", "")
      ems_clean(data,
                by = input$by,
                sds = input$sds,
                ignore_undetected = input$ignore_undetected,
                large_only = input$large_only,
                remove_blanks = input$remove_blanks,
                max_cv = max_cv)},
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
  })

  clean_rv <- reactiveValues(data = NULL)
  observe({
    clean_rv$data <- clean_data()
  })

  # observeEvent(input$table_clean_rows_selected, {
  #   clean_rv$data <- add_outlier_table(clean_rv$data, input$table_clean_rows_selected)
  # })

  observeEvent(input$plot_brush, {
    clean_rv$data <- add_outlier_brush(clean_rv$data, input$plot_brush)
  })

  clean_data2 <- reactive({
    if(input$delete_outliers){
      return(clean_rv$data[!clean_rv$data$Outlier,])
    }
    clean_rv$data
  })

  observe({
    req(clean_rv$data)
    req(clean_data())
    if(all(clean_rv$data$Outlier == clean_data()$Outlier))
      return(shinyjs::hide("clear_outliers"))
    shinyjs::show("clear_outliers")
  })

  observeEvent(input$clear_outliers, {
    clean_rv$data <- clean_data()
  })

  output$ui_table_clean <- renderUI({
    req(clean_data2())
    ems_table_output(ns('table_clean'))
  })

  output$table_clean <- DT::renderDT({
    req(clean_data2())
    ems_data_table(clean_data2())
  })

  output$plot_clean <- renderPlot({
    req(clean_data2())
    wqbc::plot_timeseries(clean_data2())
  })

  return(clean_data2)
}

## To be copied in the UI
# mod_refine_ui("refine_ui_1")

## To be copied in the server
# callModule(mod_refine_server, "refine_ui_1")

