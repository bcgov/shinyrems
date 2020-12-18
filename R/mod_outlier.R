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

#' @title   mod_outlier_ui and mod_outlier_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_outlier
#'
#' @keywords internal
mod_outlier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        class = "sidebar",
        title("Find and remove outliers") %>% helper("tab4_outlier"),
        br(),
        numericInput(ns("sds"),
          label = "Standard deviations",
          value = 10
        ) %>% helper("tab4_stddev"),
        checkboxInput(ns("ignore_undetected"), "Ignore values at or below detection limit", TRUE) %>%
          helper("tab4_ignorevalues"),
        checkboxInput(ns("large_only"), "Large values only", TRUE) %>%
          helper("tab4_largevalues"),
        checkboxInput(ns("delete_outliers"), "Remove outliers from plot", FALSE) %>%
          helper("tab4_removeoutliers"),
        numericInput(ns("point_size"),
          label = "Point Size",
          value = 1.3, min = 0, max = 10
        ),
        sliderInput(ns("plot_height"),
          label = "Plot Height",
          value = 500, min = 0, max = 1000, step = 100
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Manual outlier selection",
            br(),
            uiOutput(ns("ui_plot")),
            shinyjs::hidden(button(ns("clear_outliers"),
              label = "Undo outlier selection"
            ))
          ),
          tabPanel(
            title = "Final Data",
            br(),
            dl_group("final", ns),
            br2(),
            uiOutput(ns("ui_table_final"))
          ),
          tabPanel(
            title = "Messages",
            br(),
            help_output(ns("console_clean"))
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_outlier
#' @export
#' @keywords internal

mod_outlier_server <- function(input, output, session, clean, stand) {
  ns <- session$ns

  outlier_data <- reactive({
    req(stand$data())
    req(clean$by())
    suppressWarnings(waiter::show_butler())
    withCallingHandlers(
      {
        shinyjs::html("console_clean", "")
        x <- ems_outlier(
          x = stand$data(),
          by = clean$by(),
          max_cv = max_cv(),
          remove_blanks = FALSE,
          FUN = eval(parse(text = clean$fun())),
          sds = input$sds,
          ignore_undetected = input$ignore_undetected,
          large_only = input$large_only
        )
      },
      message = function(m) {
        shinyjs::html(id = "console_clean", html = HTML(paste(m$message, "<br>")), add = TRUE)
      }
    )
    suppressWarnings(waiter::hide_butler())
    x
  })

  outlier_rv <- reactiveValues(data = NULL)
  observe({
    outlier_rv$data <- outlier_data()
  })

  # observeEvent(input$table_clean_rows_selected, {
  #   clean_rv$data <- add_outlier_table(clean_rv$data, input$table_clean_rows_selected)
  # })

  manual_outliers <- reactive({
    which(add_outlier_brush(outlier_rv$data, input$plot_brush)$Outlier)
  })

  observeEvent(input$plot_brush, {
    outlier_rv$data <- add_outlier_brush(outlier_rv$data, input$plot_brush)
  })

  outlier_data2 <- reactive({
    if (input$delete_outliers) {
      return(outlier_rv$data[!outlier_rv$data$Outlier, ])
    }
    outlier_rv$data
  })

  max_cv <- reactive({
    maxcv(clean$max_cv())
  })

  observe({
    req(outlier_rv$data)
    if (all(outlier_rv$data$Outlier == outlier_data()$Outlier)) {
      return(shinyjs::hide("clear_outliers"))
    }
    shinyjs::show("clear_outliers")
  })

  observeEvent(input$clear_outliers, {
    outlier_rv$data <- outlier_data()
  })

  output$ui_plot <- renderUI({
    req(outlier_data2())
    if (nrow(outlier_data2()) < 1) {
      return()
    }
    tagList(
      help_text("Click and drag mouse over plot to manually select outliers.
                                             Table in 'Clean Data' tab will be automatically updated."),
      plotOutput(ns("plot_clean"), brush = brushOpts(
        id = ns("plot_brush"),
        delay = 5000
      ), height = input$plot_height)
    )
  })

  output$plot_clean <- renderPlot({
    suppressWarnings(waiter::show_butler())
    p <- plot_outlier(outlier_data2(), clean$by(), input$point_size)
    suppressWarnings(waiter::hide_butler())
    p
  })

  outlier_data3 <- reactive({
    req(outlier_data2())
    x <- outlier_data2()
    x[!x$Outlier, ]
  })

  observeEvent(input$info_sds, {
    shinyjs::toggle("div_info_sds", anim = TRUE)
  })

  observeEvent(input$info_undetected, {
    shinyjs::toggle("div_info_undetected", anim = TRUE)
  })

  observeEvent(input$info_large, {
    shinyjs::toggle("div_info_large", anim = TRUE)
  })

  observeEvent(input$info_remove, {
    shinyjs::toggle("div_info_remove", anim = TRUE)
  })

  output$ui_table_final <- renderUI({
    ems_table_output(ns("table_final"))
  })

  output$table_final <- DT::renderDT({
    ems_data_table(outlier_data3())
  })

  output$dl_final <- downloadHandler(
    filename = function() {
      paste0(input$file_final, ".csv")
    },
    content = function(file) {
      readr::write_csv(outlier_data3(), file)
    }
  )

  return(
    list(
      data = outlier_data3,
      max_cv = max_cv,
      sds = reactive({
        input$sds
      }),
      ignore_undetected = reactive({
        input$ignore_undetected
      }),
      large_only = reactive({
        input$large_only
      })
    )
  )
}

## To be copied in the UI
# mod_outlier_ui("outlier_ui_1")

## To be copied in the server
# callModule(mod_outlier_server, "outlier_ui_1")
