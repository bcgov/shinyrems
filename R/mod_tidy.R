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
mod_tidy_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 br(),
                 uiOutput(ns("ui_sample_state")),
                 uiOutput(ns("ui_sample_class")),
                 uiOutput(ns("ui_mdl_action")),
                 checkboxInput(ns("strict"), "Strict matching", value = TRUE) %>%
                   embed_help("info_strict", ns, info$strict)),
    mainPanel(tabsetPanel(tabPanel(title = "Tidy Data",
                                   br(),
                                   dl_group("tidy", ns),
                                   br2(),
                                   uiOutput(ns("ui_table_tidy"))),
                          tabPanel(title = "Messages",
                                   br(),
                                   help_output(ns("console_stand"))),
                          tabPanel(title = "R Code",
                                   wellPanel(uiOutput(ns("rcode"))))
    ))
  )
}

# Module Server

#' @rdname mod_standardise
#' @export
#' @keywords internal

mod_tidy_server <- function(input, output, session, raw){
  ns <- session$ns

  tidy_data <- reactive({
    req(raw$data())
    ems_tidy(raw$data(), input$mdl_action,
             raw$data_type(), raw$dataset(),
             raw$cols())
  })

  filter_data <- reactive({
    x <- tidy_data()
    if(nrow(x) < 1) return(empty_tidy)
    x[x$SAMPLE_STATE %in% input$sample_state & x$SAMPLE_CLASS %in% input$sample_class,]
  })

  stand_data <- reactive({
    suppressWarnings(waiter::show_butler())
    withCallingHandlers({
      shinyjs::html("console_stand", "")
      x <- ems_standardize(filter_data(), input$strict)},
      message = function(m) {
        shinyjs::html(id = "console_stand", html = HTML(paste(m$message, "<br>")), add = TRUE)
      })
    suppressWarnings(waiter::hide_butler())
    x
  })

  output$ui_sample_state <- renderUI({
    x <- sort(unique(raw$data()$SAMPLE_STATE))
    select_input_x(ns("sample_state"),
                   label = "Select values of SAMPLE_STATE to include",
                   choices = x,
                   selected = x)
  })

  output$ui_sample_class <- renderUI({
    x <- sort(unique(raw$data()$SAMPLE_CLASS))
    select_input_x(ns("sample_class"),
                   label = "Select values of SAMPLE_CLASS to include",
                   choices = x,
                   selected = x)
  })

  output$ui_mdl_action <- renderUI({
    selectInput(ns("mdl_action"), label = "MDL Action",
                choices = c("zero", "mdl", "half", "na", "none"),
                selected = "zero") %>%
      embed_help("info_mdl", ns, info$mdl_action)

  })

  observeEvent(input$info_mdl, {
    shinyjs::toggle("div_info_mdl", anim = TRUE)
  })

  observeEvent(input$info_strict, {
    shinyjs::toggle("div_info_strict", anim = TRUE)
  })

  output$ui_table_tidy <- renderUI({
    ems_table_output(ns('table_tidy'))
  })

  output$table_tidy <- DT::renderDT({
    ems_data_table(filter_data())
  })

  output$dl_tidy <- downloadHandler(
    filename = function(){
      paste0(input$file_tidy, ".csv")
    },
    content = function(file) {
      readr::write_csv(stand_data(), file)
    })

  rcodetidy <- reactive({
    rcode_tidy(input$mdl_action, raw$cols())
  })

  rcodestand <- reactive({
    rcode_standardize(input$strict)
  })

  output$rcode <- renderUI({
    print("hi")
    print(rcodestand())
    print(rcodetidy())
    tagList(rcodetidy(),
            br2(),
        rcodestand())
  })

  return(
    list(
      data = stand_data,
      rcodetidy = rcodetidy,
      rcodestand = rcodestand
    )
  )
}

