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
mod_refine_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(class = "sidebar"),

    mainPanel(tabsetPanel(selected = "Table",
                          id = ns("tabset_data"),
                          tabPanel(title = "Table",
                                   ems_table_output(ns('data_table')),
                                   downloadButton(ns("dl_data_handler"), label = NULL,
                                                  style = "visibility: hidden;")),
                          tabPanel(title = "Plot")
    ))
  )
}

# Module Server

#' @rdname mod_refine
#' @export
#' @keywords internal

mod_refine_server <- function(input, output, session, data){
  ns <- session$ns

  get_data <- reactive({
    data()
  })

  # output$view_table <- renderUI({
  #   req(get_data())
  #   ems_table_output(ns('data_table'))
  # })

  output$data_table <- DT::renderDT({
    req(get_data())
    ems_data_table(get_data())
  })

  return(get_data)
}

## To be copied in the UI
# mod_refine_ui("refine_ui_1")

## To be copied in the server
# callModule(mod_refine_server, "refine_ui_1")

