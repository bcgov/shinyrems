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
                 h4("Tidy Data"),
                 selectInput(ns("mdl_action"), label = "MDL Action",
                             choices = c("zero", "mdl", "half", "na", "none"),
                             selected = "zero"),
                 h5("Clean Data")),
    mainPanel(tabsetPanel(selected = "Table",
                          id = ns("tabset_data"),
                          tabPanel(title = "Tidy",
                                   ems_table_output(ns('table_tidy')),
                                   downloadButton(ns("dl_tidy_handler"), label = NULL,
                                                  style = "visibility: hidden;")),
                          tabPanel(title = "Clean",
                                   ems_table_output(ns('table_clean')),
                                   downloadButton(ns("dl_clean_handler"), label = NULL,
                                                  style = "visibility: hidden;")),
                          tabPanel(title = "Plot")
    ))
  )
}

# Module Server

#' @rdname mod_refine
#' @export
#' @keywords internal

mod_clean_server <- function(input, output, session, data){
  ns <- session$ns

  get_data <- reactive({
    data()
  })

  tidy_data <- reactive({
    req(get_data())
    ems_tidy(get_data(), input$mdl_action)
  })

  clean_data <- reactive({
    req(tidy_data())
    # ems_clean(tidy_data(), input)
  })

  output$table_tidy <- DT::renderDT({
    req(get_data())
    ems_data_table(tidy_data())
  })

  output$table_clean <- DT::renderDT({
    req(get_data())
    ems_data_table(clean_data())
  })

  return(get_data)
}

## To be copied in the UI
# mod_refine_ui("refine_ui_1")

## To be copied in the server
# callModule(mod_refine_server, "refine_ui_1")

