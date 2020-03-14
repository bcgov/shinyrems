# Module UI

#' @title   mod_results_ui and mod_results_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_results
#'
#' @keywords internal
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   tabsetPanel(
                     tabPanel(title = "Plot",
                              br(),
                              uiOutput(ns("ui_date_range")),
                              uiOutput(ns("ui_type")),
                              fillRow(height = 75,
                                      uiOutput(ns("ui_facet")),
                                      uiOutput(ns("ui_colour"))),
                              actionLink(ns("rename"), "Rename sites"),
                              br(),
                              uiOutput(ns("ui_rename")),
                              br(),
                              sliderInput(ns("plot_height"), label = "Plot Height",
                                          value = 500, min = 0, max = 1000, step = 100)),
                     tabPanel(title = "Guideline",
                              br(),
                              radioButtons(ns("guideline"), "Show guideline on plot",
                                           choices = c("manual", "calculated"),
                                           "manual", inline = TRUE),
                              br(),
                              tags$label("Manually set guideline"),
                              numericInput(ns("user_guideline"), label = NULL, 0),
                              br(),
                              tags$label("Calculate guideline"),
                              radioButtons(ns("term"), "Select term",
                                           choices = c("short", "long", "long-daily"),
                                           selected = "long", inline = TRUE),
                              checkboxInput(ns("estimate_variables"), "Get modelled estimate",
                                            value = FALSE),
                              actionButton(ns("get"), "Get/update guideline")
                              ))
                   ),
      mainPanel(tabsetPanel(selected = "Plot",
                            tabPanel(title = "Plot",
                                     br(),
                                     dl_group("plot", ns),
                                     br2(),
                                     uiOutput(ns("ui_plot"))),
                            tabPanel(title = "Summary Statistics",
                                     br(),
                                     dl_group("table", ns),
                                     br2(),
                                     tableOutput(ns("table"))
                                     ),
                            tabPanel(title = "R Code",
                                     br(),
                                     wellPanel(uiOutput(ns("rcode"))))
      ))
    )
  )
}

# Module Server

#' @rdname mod_results
#' @export
#' @keywords internal

mod_results_server <- function(input, output, session, data, tidy, clean, outlier){
  ns <- session$ns

  observe({
    req(input$plot_type)
    if(input$plot_type == "scatter"){
      show("div_geom")
      hide("timeframe")
    } else {
      hide("div_geom")
      show("timeframe")
    }
  })

  plots <- reactive({
    req(input$date_range)
    req(input$facet)
    req(input$colour)

    ems_plot(rv$data, input$plot_type,
              input$geom, input$date_range,
              input$point_size, input$line_size,
              input$facet, input$colour, input$timeframe,
             rv$guideline)
  })

  summary_table <- reactive({
    suppressWarnings(waiter::show_butler())
    x <- ems_summary_table(rv$data)
    suppressWarnings(waiter::hide_butler())
    x
  })

  output$table <- renderTable({
    summary_table()
  })

  output$ui_plot <- renderUI({
      plotOutput(ns("ems_plot"), height = input$plot_height)
  })

  output$ems_plot <- renderPlot({
    # suppressWarnings(waiter::show_butler())
    plots()
    # suppressWarnings(waiter::hide_butler())
  })

  output$ui_date_range <- renderUI({
    req(outlier$data())
    if(nrow(outlier$data()) < 1) return()
    date_range <- range(outlier$data()$Date, na.rm = TRUE)
    tagList(
      tags$label("Adjust plot start and end date"),
      help_text("This only changes the plot x-axis,
                not the underlying data and summary table."),
      dateRangeInput(ns("date_range"), label = NULL,
                     start = date_range[1], end = date_range[2])
    )
  })

  output$ui_type <- renderUI({
    tagList(
      radioButtons(ns("plot_type"), label = "Plot type",
                   choices = c("scatter", "boxplot"),
                   selected = "scatter", inline = TRUE),
      shinyjs::hidden(div(id = ns("div_geom"),
        checkboxGroupInput(ns("geom"), label = NULL,
                                         choices = c("show lines", "show points"),
                                         selected = c("show points", "show lines"),
                                         inline = TRUE),
        fillRow(height = 75,
                numericInput(ns("point_size"), label = "Point size", value = 1.5,
                             min = 0.1, max = 10),
                numericInput(ns("line_size"), label = "Line size", value = 0.3,
                             min = 0.1, max = 10)
        ))),
      shinyjs::hidden(selectInput(ns("timeframe"), label = "Group by time window",
                                  choices = c("Year", "Year-Month", "Month", "Season"),
                                  selected = c("Year")) %>%
                        embed_help("info_timeframe", ns, info$timeframe))
    )
  })

  output$ui_facet <- renderUI({
    data <- rv$data
    x <- sort(intersect(names(data), c("Variable", "EMS_ID")))
    selectInput(ns("facet"), "Facet by",
                choices = x,
                selected = "Variable")
  })

  output$ui_colour <- renderUI({
    data <- rv$data
    x <- sort(intersect(names(data), c("Variable", "EMS_ID")))
    selectInput(ns("colour"), "Colour by",
                choices = x,
                selected = x[1])
  })

  output$dl_plot <- downloadHandler(
    filename = function(){
      paste0(input$file_plot, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plots()[[1]], device = "png")
    })

  output$dl_table <- downloadHandler(
    filename = function(){
      paste0(input$file_table, ".csv")
    },
    content = function(file) {
      readr::write_csv(summary_table(), file)
    })

  rv <- reactiveValues(data = NULL,
                       guideline = NULL,
                       guideline_calc = NULL)
  observe({
    data <- outlier$data()
    data$EMS_ID_Renamed <- data$EMS_ID
    rv$data <- data
  })

  observeEvent(input$finalise, {
    data <- rv$data
    sites <- unique(data$EMS_ID)
    for(i in sites){
      x <- input[[i]]
      data$EMS_ID_Renamed[data$EMS_ID == i] <- x
    }
    rv$data <- data
  })

  observeEvent(input$rename, {
    shinyjs::toggle("div_rename", anim = TRUE, animType = "slide")
  })

  output$ui_rename <- renderUI({
    sites <- unique(rv$data$EMS_ID)
    shinyjs::hidden(div(id = ns("div_rename"),
        lapply(sites, rename_inputs, ns),
        button(ns("finalise"), "Rename")))
  })

  observeEvent(input$info_timeframe, {
    shinyjs::toggle("div_info_timeframe", anim = TRUE)
  })

  data_parameter <- reactive({
    data1 <- outlier$data()
    dataset <- data$dataset()
    all_data <- data$all_data()
    lookup <- data$lookup()
    ems_data_parameter(data1, all_data = all_data, dataset = dataset,
                       lookup = lookup,
                       from_date = data$date()[1], to_date = data$date()[2],
                       mdl_action = tidy$mdl_action(),
                       cols = data$cols(), strict = tidy$strict(),
                       by = clean$by(), sds = outlier$sds(),
                       ignore_undetected = outlier$ignore_undetected(),
                       large_only = outlier$large_only(),
                       remove_blanks = clean$remove_blanks(),
                       max_cv = clean$max_cv(), FUN = clean$fun(),
                       limits = wqbc::limits)
  })

  observeEvent(input$get, {
    data1 <- outlier$data()
    dataset <- data$dataset()
    all_data <- data$all_data()
    lookup <- data$lookup()

    params <- additional_parameters(data1, lookup)
    html <- waiter_html("")
    if(length(params) == 0)
      html <- waiter_html("Calculating guideline ...")
    waiter::waiter_show(html = html)

    if(length(params) != 0){
      waiter::waiter_update(html = waiter_html(paste("Fetching additional data:",
                                                   paste(params, collapse = ", "))))
      data2 <- data_parameter()
      all_data <- rbind(data1, data2)
    } else {
      all_data <- data1
    }

    waiter::waiter_update(html = waiter_html("Calculating guideline ..."))
    x <- try(wqbc::calc_limits(all_data, clean = FALSE, term = input$term,
                           estimate_variables = input$estimate_variables), silent = TRUE)

    waiter::waiter_hide()

    if(!is_try_error(x)){
      if(nrow(x) == 0){
        return(showModal(guideline_modal()))
      }
      return(rv$guideline_calc <- x)
    } else {
      return(showModal(guideline_modal()))
    }
  })

  observe({
    if(input$guideline == "manual"){
      rv$guideline <- input$user_guideline
    } else {
      rv$guideline <- rv$guideline_calc
    }
  })

  rcodeplot <- reactive({})

  rcodetable <- reactive({})

  return(
    list(
      facet = reactive({input$facet}),
      colour = reactive({input$colour}),
      rcodeplot = rcodeplot,
      rcodetable = rcodetable
    )
  )
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")

