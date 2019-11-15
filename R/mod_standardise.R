# Module UI

#' @title   mod_standardise_ui and mod_standardise_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_standardise
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_standardise_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 checkboxInput(ns("strict"), "Strict matching", value = TRUE),
                 dl_button(ns("dl_stand"), "Download Standardized Data")),
    mainPanel(tabsetPanel(selected = "Standardized Data",
                          tabPanel(title = "Standardized Data",
                                   uiOutput(ns("ui_table_stand"))),
                          tabPanel(title = "Messages",
                                   br(),
                                   help_output(ns("console_stand")))
    ))
  )
}

# Module Server

#' @rdname mod_standardise
#' @export
#' @keywords internal

mod_standardise_server <- function(input, output, session, tidy_data){
  ns <- session$ns

  stand_data <- reactive({
    if(nrow(tidy_data()) < 1) return()
    waiter::show_butler()
    withCallingHandlers({
      shinyjs::html("console_stand", "")
      x <- ems_standardize(tidy_data(), input$strict)},
    message = function(m) {
      shinyjs::html(id = "console_stand", html = HTML(paste(m$message, "<br>")), add = TRUE)
    })
    waiter::hide_butler()
    x
  })

  output$dl_stand <- downloadHandler(
    filename = function() "ems_standardized_data.csv",
    content = function(file) {
      readr::write_csv(stand_data(), file)
    })

  output$ui_table_stand <- renderUI({
    ems_table_output(ns('table_stand'))
  })

  output$table_stand <- DT::renderDT({
    ems_data_table(stand_data())
  })

  return(stand_data)
}

