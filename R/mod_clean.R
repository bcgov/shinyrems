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
                 shinyjs::hidden(button(ns("dl_clean"), "Download Clean Data"))),
    mainPanel(tabsetPanel(selected = "Table",
                          id = ns("tabset_data"),
                          tabPanel(title = "Table",
                                   uiOutput(ns("ui_table_clean"))),
                          tabPanel(title = "Plot")
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
    if(!is.null(clean_data()))
      return(show("dl_clean"))
    hide("dl_clean")
  })

  clean_data <- reactive({
    req(stand_data())
    ems_clean(stand_data())
  })

  output$ui_table_clean <- renderUI({
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

