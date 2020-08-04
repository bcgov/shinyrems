# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

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
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        class = "sidebar",
        tabsetPanel(
          tabPanel(
            title = "Plot",
            br(),
            uiOutput(ns("ui_date_range")),
            uiOutput(ns("ui_type")),
            fillRow(
              height = 75,
              uiOutput(ns("ui_facet")),
              uiOutput(ns("ui_colour"))
            ),
            button(ns("rename"), "Rename Stations"),
            br(),
            br(),
            sliderInput(ns("plot_height"),
              label = "Plot Height",
              value = 500, min = 0, max = 1000, step = 100
            ),
            selectInput(ns("palette"), label = "Palette",
                        choices = c("Accent", "Dark2", "Paired", "Pastel1",
                                    "Pastel2", "Set1", "Set2", "Set3"))
          ),
          tabPanel(
            title = "Guideline",
            br(),
            radioButtons(ns("guideline"), "How do you want to determine Water Quality Guideline?",
              choices = c("set manually", "calculate from data"),
              "set manually", inline = TRUE
            ),
            shinyjs::hidden(div(
              id = ns("div_manual"),
              div(inline(p("Find a guideline using the")),
                  inline(tags$a("Water Quality Guideline app",
                                href = "https://bcgov-env.shinyapps.io/bc_wqg/",
                                target = "_blank"))),
              numericInput(ns("user_guideline"), label = NULL, value = NULL)
            )),
            shinyjs::hidden(div(
              id = ns("div_calculate"),
              radioButtons(ns("term"), "Select term",
                choices = c("short", "long", "long-daily"),
                selected = "long", inline = TRUE
              ),
              checkboxInput(ns("estimate_variables"), "Get modelled estimate",
                value = FALSE
              ),
              actionButton(ns("get"), "Get/update guideline")
            ))
          ),
          tabPanel(
            title = "Summary Table",
            checkboxInput(ns("censored"),
              label = "Account for data censoring", value = TRUE
            ),
            checkboxInput(ns("narm"),
              label = "Exclude missing values", value = TRUE
            ),
            uiOutput(ns("ui_by")),
            numericInput(ns("sigfig"),
              label = "Significant figures",
              value = 2, min = 0, max = 10
            )
          )
        )
      ),
      mainPanel(tabsetPanel(
        selected = "Plot",
        tabPanel(
          title = "Plot",
          br(),
          fillRow(
            height = "90%", width = 650, flex = c(2, 3, 1, 1, 1, 1, 0.7, 1.5),
            dl_button(ns("dl_plot"), "Download"),
            textInput(ns("dl_file"), label = NULL, value = "", placeholder = "file name"),
            p(HTML("width&nbsp"), style = "text-align: right;"),
            numericInput(ns("dl_width"), label = NULL, value = 9),
            p(HTML("height&nbsp"), style = "text-align: right;"),
            numericInput(ns("dl_height"), label = NULL, value = 6),
            p(HTML("dpi&nbsp"), style = "text-align: right;"),
            numericInput(ns("dl_dpi"), label = NULL, value = 300)
          ),
          br2(),
          uiOutput(ns("ui_plot"))
        ),
        tabPanel(
          title = "Summary Table",
          br(),
          dl_group("table", ns),
          br2(), br(),
          ems_table_output(ns("table"))
        ),
        tabPanel(
          title = "Data (with guideline)",
          br(),
          dl_group("final_table", ns),
          br2(), br(),
          ems_table_output(ns("final_table"))
        )
      ))
    )
  )
}

# Module Server

#' @rdname mod_results
#' @export
#' @keywords internal

mod_results_server <- function(input, output, session, data, tidy, clean, outlier) {
  ns <- session$ns

  observe({
    req(input$plot_type)
    if (input$plot_type == "scatter") {
      show("div_geom")
      hide("timeframe")
    } else {
      hide("div_geom")
      show("timeframe")
    }
  })

  observe({
    if (input$guideline == "set manually") {
      show("div_manual")
      hide("div_calculate")
    } else {
      hide("div_manual")
      show("div_calculate")
    }
  })

  plot <- reactive({
    req(input$date_range)
    req(input$facet)
    req(input$colour)

    data <- rv$data

    data <- ems_plot_data(data = rv$data, date_range = input$date_range,
                          timeframe = input$timeframe)
    gp <- ems_plot_base(data, facet = input$facet) %>%
      ems_plot_add_geom(plot_type = input$plot_type, geom = input$geom,
                        point_size = input$point_size, line_size = input$line_size,
                        colour = input$colour, timeframe = input$timeframe,
                        palette = input$palette)
    if(!is.null(rv$guideline))
      gp <- gp %>% ems_plot_add_guideline(guideline = rv$guideline)

    gp
  })

  summary_table <- reactive({
    suppressWarnings(waiter::show_butler())
    x <- wqbc::summarise_wqdata(rv$data,
      by = input$by,
      censored = input$censored,
      na.rm = input$narm
    ) %>%
      dplyr::mutate_if(is.numeric, function(x) signif(x, input$sigfig))
    suppressWarnings(waiter::hide_butler())
    x
  })

  output$table <- DT::renderDT({
    DT::datatable(summary_table(),
      class = "cell-border stripe compact",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "t",
        ordering = FALSE
      )
    )
  })

  output$ui_plot <- renderUI({
    plotOutput(ns("ems_plot"), height = input$plot_height)
  })

  output$ui_site_type <- renderUI({
    data <- rv$data
    x <- sort(intersect(names(data), c("Station", "EMS_ID")))
    if(length(x) < 2) return()
    radioButtons(ns("site_type"), label = "Label sites by",
                 choices = x, inline = TRUE)
  })

  output$ems_plot <- renderPlot({
    plot()
  })

  output$ui_date_range <- renderUI({
    req(outlier$data())
    if (nrow(outlier$data()) < 1) {
      return()
    }
    date_range <- range(outlier$data()$Date, na.rm = TRUE)
    tagList(
      tags$label("Adjust plot start and end date"),
      help_text("This only changes the plot x-axis,
                not the underlying data and summary table."),
      dateRangeInput(ns("date_range"),
        label = NULL,
        start = date_range[1], end = date_range[2]
      )
    )
  })

  output$ui_type <- renderUI({
    tagList(
      radioButtons(ns("plot_type"),
        label = "Plot type",
        choices = c("scatter", "boxplot"),
        selected = "scatter", inline = TRUE
      ),
      shinyjs::hidden(div(
        id = ns("div_geom"),
        checkboxGroupInput(ns("geom"),
          label = NULL,
          choices = c("show lines", "show points"),
          selected = c("show points"),
          inline = TRUE
        ),
        fillRow(
          height = 75,
          numericInput(ns("point_size"),
            label = "Point size", value = 1.5,
            min = 0.1, max = 10
          ),
          numericInput(ns("line_size"),
            label = "Line size", value = 0.3,
            min = 0.1, max = 10
          )
        )
      )),
      shinyjs::hidden(selectInput(ns("timeframe"),
        label = "Group by time window",
        choices = c("Year", "Year-Month", "Month", "Season"),
        selected = c("Year")
      ) %>%
        embed_help("info_timeframe", ns, info$timeframe))
    )
  })

  output$ui_facet <- renderUI({
    data <- rv$data
    x <- sort(intersect(names(data), c("Station", "Variable", "EMS_ID")))
    selectInput(ns("facet"), "Facet by",
      choices = x,
      selected = "Variable"
    )
  })

  output$ui_colour <- renderUI({
    data <- rv$data
    colour_vars <- c("Station", "Variable", "EMS_ID", "LOWER_DEPTH", "UPPER_DEPTH")
    x <- sort(intersect(names(data), colour_vars))
    selectInput(ns("colour"), "Colour by",
      choices = x,
      selected = x[1]
    )
  })

  output$dl_plot <- downloadHandler(
    filename = function() {
      paste0(input$dl_file, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot(),
                      width = input$dl_width,
                      height = input$dl_height,
                      dpi = input$dl_dpi,
                      device = "png")
    }
  )

  output$dl_table <- downloadHandler(
    filename = function() {
      paste0(input$file_table, ".csv")
    },
    content = function(file) {
      readr::write_csv(summary_table(), file)
    }
  )

  rv <- reactiveValues(
    data = NULL,
    guideline = NULL,
    guideline_calc = NULL
  )

  observe({
    data <- outlier$data()
    data$Site_Renamed <- data$Station
    rv$data <- data
  })

  observeEvent(input$finalise, {
    data <- rv$data
    sites <- unique(data$Station)
    for (i in sites) {
      x <- input[[i]]
      data$Site_Renamed[data$Station == i] <- x
    }
    removeModal()
    rv$data <- data
  })

  observeEvent(input$rename, {
    showModal(modalDialog(uiOutput(ns("ui_rename"))))
  })

  output$ui_rename <- renderUI({
    sites <- unique(rv$data$Station)
    div(
      lapply(sites, rename_inputs, ns),
      button(ns("finalise"), "Rename")
    )
  })

  output$ui_by <- renderUI({
    select_input_x(ns("by"),
      label = "Summarise by columns",
      choices = clean$by(),
      selected = clean$by()
    )
  })
  outputOptions(output, "ui_by", suspendWhenHidden = FALSE)

  observeEvent(input$info_timeframe, {
    shinyjs::toggle("div_info_timeframe", anim = TRUE)
  })

  data_parameter <- reactive({
    data1 <- outlier$data()
    dataset <- data$dataset()
    all_data <- data$all_data()
    lookup <- data$lookup()
    ems_data_parameter(data1,
      all_data = all_data, dataset = dataset,
      lookup = lookup,
      from_date = data$date()[1], to_date = data$date()[2],
      mdl_action = tidy$mdl_action(),
      cols = data$cols(), strict = tidy$strict(),
      by = clean$by(), sds = outlier$sds(),
      ignore_undetected = outlier$ignore_undetected(),
      large_only = outlier$large_only(),
      remove_blanks = clean$remove_blanks(),
      max_cv = clean$max_cv(), FUN = eval(parse(text = clean$fun())),
      limits = wqbc::limits
    )
  })

  observeEvent(input$get, {
    data1 <- outlier$data()
    dataset <- data$dataset()
    all_data <- data$all_data()
    lookup <- data$lookup()

    params <- additional_parameters(data1, lookup)
    html <- waiter_html("")
    if (length(params) == 0) {
      html <- waiter_html("Calculating guideline ...")
    }
    waiter::waiter_show(html = html)

    if (length(params) != 0) {
      waiter::waiter_update(html = waiter_html(paste(
        "Fetching additional data:",
        paste(params, collapse = ", ")
      )))
      data2 <- data_parameter()
      all_data <- rbind(data1, data2)
    } else {
      all_data <- data1
    }

    waiter::waiter_update(html = waiter_html("Calculating guideline ..."))
    x <- try(wqbc::calc_limits(all_data,
      clean = FALSE, term = input$term,
      estimate_variables = input$estimate_variables
    ), silent = TRUE)

    waiter::waiter_hide()

    if (!is_try_error(x)) {
      if (nrow(x) == 0) {
        return(showModal(guideline_modal()))
      }
      return(rv$guideline_calc <- x)
    } else {
      return(showModal(guideline_modal()))
    }
  })

  observe({
    if (input$guideline == "set manually") {
      req(input$user_guideline)
      rv$guideline <- data.frame(UpperLimit = input$user_guideline)
    } else {
      print(rv$guideline_calc)
      rv$guideline <- rv$guideline_calc
    }
  })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")
