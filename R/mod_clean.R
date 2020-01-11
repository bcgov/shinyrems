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
                 checkboxInput(ns("remove_blanks"), "Remove blanks", value = TRUE),
                 uiOutput(ns("ui_by")),
                 radioButtons(ns("fun"), label = "Aggreggation function",
                              choices = c("mean", "median", "max"),
                              selected = "max", inline = TRUE),
                 numericInput(ns("max_cv"), label = "Maximum CV", value = Inf) %>%
                   embed_help("info_maxcv", ns, info$max_cv),
                 dl_button(ns("dl_clean"), "Download Clean Data")),
    mainPanel(tabsetPanel(selected = "Clean Data",
                          id = ns("tabset_data"),
                          tabPanel(title = "Clean Data",
                                   uiOutput(ns("ui_table_clean"))),
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

  output$ui_by <- renderUI({
    data <- stand_data()
    if(nrow(data) < 1) return()
    selected <- intersect(names(data),
                     c("EMS_ID", "UPPER_DEPTH", "LOWER_DEPTH"))
    optional <- intersect(names(data),
                          c("SAMPLE_STATE", "SAMPLE_CLASS"))

    select_input_x(ns("by"), label = "Aggregation columns",
                   choices = c(selected, optional),
                   selected = selected)
  })

  max_cv <- reactive(
    maxcv(input$max_cv)
  )

  clean_data <- reactive({
    waiter::show_butler()
    withCallingHandlers({
      shinyjs::html("console_clean", "")
      x <- ems_aggregate(stand_data(),
                by = input$by,
                remove_blanks = input$remove_blanks,
                max_cv = max_cv(),
                FUN = input$fun)},
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
    # print(x)
    waiter::hide_butler()
    x
  })

  output$ui_table_clean <- renderUI({
    ems_table_output(ns('table_clean'))
  })

  output$table_clean <- DT::renderDT({
    ems_data_table(clean_data())
  })

  output$dl_clean <- downloadHandler(
    filename = function() "ems_clean_data.csv",
    content = function(file) {
      readr::write_csv(clean_data(), file)
    })

  observeEvent(input$info_maxcv, {
    shinyjs::toggle("div_info_maxcv", anim = TRUE)
  })

  return(
    list(
      by = reactive({input$by}),
      fun = reactive({input$fun}),
      remove_blanks = reactive({input$remove_blanks}),
      max_cv = reactive({input$max_cv})
    )
  )

}

## To be copied in the UI
# mod_refine_ui("refine_ui_1")

## To be copied in the server
# callModule(mod_refine_server, "refine_ui_1")

