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
                 tags$label("Console ouptut"),
                 help_output(ns("console_stand")),
                 br(),
                 button(ns("dl_stand"), "Download Standardized Data")),
    mainPanel(tabsetPanel(selected = "Standardized Data",
                          tabPanel(title = "Standardized Data",
                                   uiOutput(ns("ui_table_stand")))
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

    withCallingHandlers({
      shinyjs::html("console_stand", "")
      ems_standardize(tidy_data(), input$strict)},

    message = function(m) {
      shinyjs::html(id = "console_stand", html = HTML(paste(m$message, "<br>")), add = TRUE)
    })
  })

  observe({
    show_hide(stand_data(), "dl_stand")
  })

  output$dl_stand <- downloadHandler(
    filename = function() "ems_standardized_data.csv",
    content = function(file) {
      readr::write_csv(stand_data(), file)
    })

  output$ui_table_stand <- renderUI({
    req(stand_data)
    ems_data_table(stand_data())
    ems_table_output(ns('table_stand'))
  })

  output$table_stand <- DT::renderDT({
    ems_data_table(stand_data())
  })

  return(stand_data)
}

## To be copied in the UI
# mod_standardise_ui("standardise_ui_1")

## To be copied in the server
# callModule(mod_standardise_server, "standardise_ui_1")

