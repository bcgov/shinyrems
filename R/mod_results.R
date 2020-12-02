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
                                    "Pastel2", "Set1", "Set2", "Set3")),
            numericInput(ns("ncol"), "Number of columns",
                         value = 1, min = 1, max = 20) %>%
              helper("tab5_ncol"),
            checkboxInput(ns("scales"), label = "Standardize Y-axis scales", value = TRUE) %>%
              helper("tab5_scales")
          ),
          tabPanel(
            title = "Guideline",
            br(),
            fillRow(actionButton(ns("add_manual"), "Add manual"),
                    actionButton(ns("add_calculated"), "Add calculated"),
                    height = "40px", width = 220, flex = c(1, 1)) %>% helper("tab5_guideline"),
            shinyWidgets::colorSelectorInput(ns("guideline_colour"),
                                             choices = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
                                             label = "Guideline colour", selected = "black"),
            br(),
            fluidRow(div(id = ns("empty"))),
          ),
          tabPanel(
            title = "Summary Table",
            h4("Create summary table") %>% helper("tab5_summary"),
            checkboxInput(ns("censored"),
              label = "Account for data censoring", value = TRUE
            ) %>% helper("tab5_censoring"),
            checkboxInput(ns("narm"),
              label = "Exclude missing values", value = TRUE
            ) %>% helper("tab5_narm"),
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
          title = "Guideline Data",
          br(),
          dl_group("final_table", ns),
          br2(),
          numericInput(ns("guideline_sigfig"),
                       label = "Guideline significant figures",
                       value = 2, min = 0, max = 10
          ), br(),
          help_text("Only calculated guidelines are shown."),
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

  rv <- reactiveValues(
    data = NULL,
    guideline = NULL,
    guideline_final = NULL
  )

  observe({
    req(input$plot_type)
    if (input$plot_type == "scatter") {
      show("div_geom")
      hide("div_timeframe")
    } else {
      hide("div_geom")
      show("div_timeframe")
    }
  })

  observe({
    outlier$data()
    rv$guideline <- NULL
  })

  plot <- reactive({
    req(input$date_range)
    req(input$facet)
    req(input$colour)

    data <- rv$data

    data <- ems_plot_data(data = rv$data, date_range = input$date_range,
                          timeframe = input$timeframe)
    gp <- ems_plot_base(data, facet = input$facet, ncol = input$ncol, scales = input$scales) %>%
      ems_plot_add_geom(plot_type = input$plot_type, geom = input$geom,
                        point_size = input$point_size, line_size = input$line_size,
                        colour = input$colour, timeframe = input$timeframe,
                        palette = input$palette)

    if(!is.null(rv$guideline)){
      x <- rv$guideline
      x <- x[c("Date", "Guideline", "Variable", "UpperLimit")]
      gp <- gp %>% ems_plot_add_guideline(guideline = x, guideline_colour = input$guideline_colour)
    }

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

  output$final_table <- DT::renderDT({
    req(rv$guideline_final)
    DT::datatable(rv$guideline_final,
                  class = "cell-border stripe compact",
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE,
                    # dom = "t",
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
      shinyjs::hidden(div(
        id = ns("div_timeframe"),
        selectInput(ns("timeframe"),
        label = "Group by time window",
        choices = c("Year", "Year-Month", "Month", "Season"),
        selected = c("Year")
      ))) %>%
        helper("tab5_timeframe")
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

  observeEvent(input$add_manual, {
    if(input$add_manual > 3)
      return(showModal(guideline_modal("Only 3 manual guidelines are allowed.")))
    insertUI(
      selector = paste0("#", ns("empty")),
      where = "beforeEnd",
      ui = tagList(
        subtitle(paste("Manual guideline", input$add_manual)),
        fillRow(
          textInput(ns(paste0("manual_name_", input$add_manual)), label = NULL,
                    placeholder = "guideline name ..."),
          numericInput(ns(paste0("manual_", input$add_manual)),
                       label = NULL, value = NULL),
          actionButton(ns(paste0("add_manual_", input$add_manual)), "Add/update"),
          flex = c(1.5, 1, 1),
          height = "40px"
        ),
      )
    )
  })

  observeEvent(input$add_calculated, {
    if(input$add_calculated > 3)
      return(showModal(guideline_modal("Only 3 calculated guidelines are allowed.")))
    insertUI(
      selector = paste0("#", ns("empty")),
      where = "beforeEnd",
      ui = tagList(
        subtitle(paste("Calculated guideline", input$add_calculated)),
        fillRow(
          textInput(ns(paste0("calculated_name_", input$add_calculated)), label = NULL,
                    placeholder = "guideline name ..."),
          selectInput(ns(paste0("term_", input$add_calculated)),
                      label = NULL,
                      choices = c("short term" = "short", "long term" = "long",
                                  "long-daily term" = "long-daily"),
                      selected = "short"),
          actionButton(ns(paste0("add_calculated_", input$add_calculated)), "Add/update"),
          flex = c(1.5, 1, 1),
          height = "40px"
        ),
        checkboxInput(ns(paste0("estimate_", input$add_calculated)), "Get modelled estimate",
                      value = FALSE
        ),
      )
    )
  })

  observeEvent(input$add_manual_1, {
    req(input$manual_1)
    req(input$manual_name_1)
    rv$guideline <- add_manual_guideline(rv$guideline, rv$data,
                                         input$manual_1,
                                         input$manual_name_1,
                                         "manual_1")
  })

  observeEvent(input$add_manual_2, {
    req(input$manual_2)
    req(input$manual_name_2)
    rv$guideline <- add_manual_guideline(rv$guideline, rv$data,
                                         input$manual_2,
                                         input$manual_name_2,
                                         "manual_2")
  })

  observeEvent(input$add_manual_3, {
    req(input$manual_3)
    req(input$manual_name_3)
    rv$guideline <- add_manual_guideline(rv$guideline, rv$data,
                                         input$manual_3,
                                         input$manual_name_3,
                                         "manual_3")
  })

  observeEvent(input$add_calculated_1, {
    req(input$calculated_name_1)
    req(input$term_1)
    req(input$guideline_sigfig)
    waiter::waiter_show(html = waiter_html("Calculating guideline ..."))
    id <- "calculated_1"
    x <- try(add_calculated_guideline(data = outlier$data(),
                             dataset = data$dataset(),
                             all_data = data$all_data(),
                             lookup = data$lookup(),
                             name = input$calculated_name_1,
                             id = id,
                             term = input$term_1,
                             estimate = input$estimate_1,
                             sigfig = input$guideline_sigfig,
                             from_date = data$date()[1],
                             to_date = data$date()[2],
                             mdl_action = tidy$mdl_action(),
                             cols = data$cols(),
                             strict = tidy$strict(),
                             by = clean$by(),
                             sds = outlier$sds(),
                             ignore_undetected = outlier$ignore_undetected(),
                             large_only = outlier$large_only(),
                             max_cv = clean$max_cv(),
                             fun = clean$fun()), silent = TRUE)
    if(is_try_error(x)){
      waiter::waiter_hide()
      return(showModal(guideline_modal(x)))
    }
    y <- rv$guideline
    y <- y[y$id != id,]
    rv$guideline <- dplyr::bind_rows(y, x)
    waiter::waiter_hide()
  })

  observeEvent(input$add_calculated_2, {
    req(input$calculated_name_2)
    req(input$term_2)
    req(input$guideline_sigfig)
    waiter::waiter_show(html = waiter_html("Calculating guideline ..."))
    id <- "calculated_2"
    x <- try(add_calculated_guideline(data = outlier$data(),
                                      dataset = data$dataset(),
                                      all_data = data$all_data(),
                                      lookup = data$lookup(),
                                      name = input$calculated_name_2,
                                      id = id,
                                      term = input$term_2,
                                      estimate = input$estimate_2,
                                      sigfig = input$guideline_sigfig,
                                      from_date = data$date()[1],
                                      to_date = data$date()[2],
                                      mdl_action = tidy$mdl_action(),
                                      cols = data$cols(),
                                      strict = tidy$strict(),
                                      by = clean$by(),
                                      sds = outlier$sds(),
                                      ignore_undetected = outlier$ignore_undetected(),
                                      large_only = outlier$large_only(),
                                      max_cv = clean$max_cv(),
                                      fun = clean$fun()), silent = TRUE)
    if(is_try_error(x)){
      waiter::waiter_hide()
      return(showModal(guideline_modal(x)))
    }
    y <- rv$guideline
    y <- y[y$id != id,]
    rv$guideline <- dplyr::bind_rows(y, x)
    waiter::waiter_hide()
  })

  observeEvent(input$add_calculated_3, {
    req(input$calculated_name_3)
    req(input$term_3)
    req(input$guideline_sigfig)
    waiter::waiter_show(html = waiter_html("Calculating guideline ..."))
    id <- "calculated_3"
    x <- try(add_calculated_guideline(data = outlier$data(),
                                      dataset = data$dataset(),
                                      all_data = data$all_data(),
                                      lookup = data$lookup(),
                                      name = input$calculated_name_3,
                                      id = id,
                                      term = input$term_3,
                                      estimate = input$estimate_3,
                                      sigfig = input$guideline_sigfig,
                                      from_date = data$date()[1],
                                      to_date = data$date()[2],
                                      mdl_action = tidy$mdl_action(),
                                      cols = data$cols(),
                                      strict = tidy$strict(),
                                      by = clean$by(),
                                      sds = outlier$sds(),
                                      ignore_undetected = outlier$ignore_undetected(),
                                      large_only = outlier$large_only(),
                                      max_cv = clean$max_cv(),
                                      fun = clean$fun()), silent = TRUE)
    if(is_try_error(x)){
      waiter::waiter_hide()
      return(showModal(guideline_modal(x)))
    }
    y <- rv$guideline
    y <- y[y$id != id,]
    rv$guideline <- dplyr::bind_rows(y, x)
    waiter::waiter_hide()
  })

  observe({
    req(rv$guideline)
    req(input$guideline_sigfig)
    x <- rv$guideline
    x$UpperLimit <- signif(x$UpperLimit, input$guideline_sigfig)
    x$id <- NULL
    x <- x[x$calculated,]
    x$calculated <- NULL
    rv$guideline_final <- x
  })
}

## To be copied in the UI
# mod_results_ui("results_ui_1")

## To be copied in the server
# callModule(mod_results_server, "results_ui_1")
