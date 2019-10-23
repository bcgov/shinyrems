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
                 checkboxInput(ns("remove_blanks"), "Remove blanks", value = FALSE),
                 h4("Daily aggregation by columns"),
                 select_input_x(ns("by"), label = NULL,
                                choices = c("EMS_ID", "Station", "SAMPLE_STATE",
                                            "SAMPLE_CLASS", "SAMPLE_DESCRIPTOR",
                                            "LOCATION_TYPE"),
                                selected = "Station"),
                 h4("Automatic outlier detection"),
                 numericInput(ns("sds"), label = "Number of standard deviations",
                              value = 10),
                 checkboxInput(ns("ignore_undetected"), "Ignore undetected", TRUE),
                 checkboxInput(ns("large_only"), "Large values only", TRUE),
                 checkboxInput(ns("delete_outliers"), "Detele outliers", FALSE),
                 shinyjs::hidden(button(ns("dl_clean"), "Download Clean Data"))),
    mainPanel(tabsetPanel(selected = "Table",
                          id = ns("tabset_data"),
                          tabPanel(title = "Table",
                                   uiOutput(ns("ui_table_clean"))),
                          tabPanel(title = "Plot"),
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

  clean_data <- reactive({
    req(stand_data())
    req(input$by)
    withCallingHandlers({
      shinyjs::html("console_clean", "")
      ems_clean(stand_data(), by = input$by, sds = input$sds,
                ignore_undetected = input$ignore_undetected,
                large_only = input$large_only,
                delete_outliers = input$delete_outliers,
                remove_blanks = input$remove_blanks)},
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
  })

  output$ui_table_clean <- renderUI({
    req(clean_data())
    ems_table_output(ns('table_clean'))
  })

  output$table_clean <- DT::renderDT({
    req(clean_data())
    ems_data_table(clean_data())
  })

  return(clean_data)
}

## To be copied in the UI
# mod_refine_ui("refine_ui_1")

## To be copied in the server
# callModule(mod_refine_server, "refine_ui_1")

