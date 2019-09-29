# Module UI

#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4, class = 'sidebar',
                   checkboxInput(ns("check_permit"),
                                 label = "Filter by Permit Number",
                                 value = FALSE),
                   uiOutput(ns("ui_permit")),
                   tags$label("Select Site(s)"),
                   radioButtons(ns("site_type"), label = NULL,
                                choices = c("Monitoring Location", "EMS ID"),
                                selected = "Monitoring Location", inline = TRUE),
                   uiOutput(ns("ui_site")),
                   tags$label("Select Parameter(s)"),
                   radioButtons(ns("parameter_type"), label = NULL,
                                choices = c("Parameter Name", "Parameter Code"),
                                selected = "Parameter Name", inline = TRUE),
                   uiOutput(ns("ui_parameter")),
                   uiOutput(ns("ui_date")),
                   br(),
                   button(ns('dl_data'), label = "Get Data (csv)"),
                   # invisible download handlers so can use bootstrap buttons
                   downloadButton(ns("dl_data_handler"), label = NULL,
                                  style = "visibility: hidden;")),

      mainPanel(width = 8,
                ems_table_output(ns('data_table'))
                )
  ))
}

# Module Server

#' @rdname mod_data
#' @export
#' @keywords internal

mod_data_server <- function(input, output, session){
  ns <- session$ns
  run_mode <- getShinyOption("run_mode", "demo")

  observe({
    if(is.null(input$site) ||
       is.null(input$parameter) ||
       input$parameter == ""){
      shinyjs::disable("dl_data")
    } else {
      shinyjs::enable("dl_data")
    }
  })

  observe({
    if(!input$check_permit){
      shinyjs::hide("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$permit <- NULL
    } else {
      shinyjs::show("ui_permit", anim = TRUE, animType = "slide")
      permit_rv$permit <- input$permit
    }
  })

  lookup <- reactive({
    run_mode_lookup(run_mode)
  })

  permit_rv <- reactiveValues(permit = NULL)
  observeEvent(input$permit, {
    permit_rv$permit <- input$permit
  })

  get_permits <- reactive({
    if(!input$check_permit){
      return(NULL)
    }
    permits(lookup())
  })

  get_sites <- reactive({
    lookup <- lookup()
    if(input$site_type == "EMS ID"){
      return(permit_sites(permit_rv$permit, lookup))
    }

    x <- permit_sites(permit_rv$permit, lookup)
    names(x) <- monitoring_locations(x, lookup)
    x
  })

  get_data <- reactive({
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    ems_data_table(get_run_mode_data(input$parameter, input$site,
                                     input$date_range[1], input$date_range[2],
                                     run_mode))
  })

  message("confirm that monitoring location always matches")

  get_parameters <- reactive({
    lookup <- lookup()
    if(input$parameter_type == "Parameter Code"){
      return(site_parameters(input$site, lookup))
    }

    x <- site_parameters(input$site, lookup)
    names(x) <- parameter_names(x, lookup)
    x
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"), label = "Permit number:",
                   choices = get_permits())
  })

  output$ui_site <- renderUI({
    select_input_x(ns("site"), label = NULL,
                 choices = get_sites())
  })

  output$ui_parameter <- renderUI({
    select_input_x(ns("parameter"),
                label = NULL,
                choices = get_parameters())
  })

  output$data_table <- DT::renderDT({
    get_data()
  })

  output$ui_date <- renderUI({
    req(input$parameter)
    req(input$site)
    dates <- date_range(input$site, input$parameter, lookup())
    dateRangeInput(ns("date_range"),
                   label = "Get any available data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2])
  })
}


