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
mod_tidy_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      title("Tidy your data") %>% helper("tab2_tidy"),
      br(),
      uiOutput(ns("ui_sample_state")) %>% helper("tab2_samplestate"),
      uiOutput(ns("ui_sample_class")) %>% helper("tab2_sampleclass"),
      uiOutput(ns("ui_mdl_action"))
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        title = "Tidy Data",
        br(),
        dl_group("tidy", ns),
        br2(),
        uiOutput(ns("ui_table_tidy"))
      ),
      tabPanel(
        title = "Messages",
        br(),
        help_output(ns("console_stand"))
      )
    ))
  )
}

# Module Server

#' @rdname mod_standardise
#' @export
#' @keywords internal

mod_tidy_server <- function(input, output, session, raw) {
  ns <- session$ns

  tidy_data <- reactive({
    req(raw$data())
    if(raw$dataset() == "upload"){
      return(raw$data())
    }
    x <- ems_tidy(
      raw$data(),
      mdl_action = input$mdl_action,
      dataset = raw$dataset(),
      cols = raw$cols()
    )
    print(raw$data())
    print(x)
    x
  })

  filter_data <- reactive({
    x <- tidy_data()
    if (nrow(x) < 1) {
      return(empty_tidy)
    }
    if("SAMPLE_STATE" %in% names(x)){
      x <- x[x$SAMPLE_STATE %in% input$sample_state,]
    }
    if("SAMPLE_CLASS" %in% names(x)){
      x <- x[x$SAMPLE_CLASS %in% input$sample_class,]
    }
    x
  })

  stand_data <- reactive({
    suppressWarnings(waiter::waiter_show())
    withCallingHandlers(
      {
        shinyjs::html("console_stand", "")
        x <- ems_standardize(filter_data(), strict = TRUE)
      },
      message = function(m) {
        shinyjs::html(id = "console_stand", html = HTML(paste(m$message, "<br>")), add = TRUE)
      }
    )
    suppressWarnings(waiter::waiter_hide())
    x
  })

  output$ui_sample_state <- renderUI({
    req("SAMPLE_STATE" %in% names(raw$data()))
    x <- sort(unique(raw$data()$SAMPLE_STATE))
    select_input_x(ns("sample_state"),
      label = "Select values of SAMPLE_STATE to include",
      choices = x,
      selected = x
    )
  })

  output$ui_sample_class <- renderUI({
    req("SAMPLE_CLASS" %in% names(raw$data()))
    x <- sort(unique(raw$data()$SAMPLE_CLASS))
    select_input_x(ns("sample_class"),
      label = "Select values of SAMPLE_CLASS to include",
      choices = x,
      selected = x
    )
  })

  output$ui_mdl_action <- renderUI({
    selectInput(ns("mdl_action"),
      label = "Set values at or below detection limit to",
      choices = c("zero" = "zero", "detection limit" = "mdl",
                  "half the detection limit" = "half",
                  "missing value (NA)" = "na"),
      selected = "zero"
    ) %>%
      helper("tab2_mdlaction")
  })

  observeEvent(input$info_mdl, {
    shinyjs::toggle("div_info_mdl", anim = TRUE)
  })

  observeEvent(input$info_strict, {
    shinyjs::toggle("div_info_strict", anim = TRUE)
  })

  output$ui_table_tidy <- renderUI({
    ems_table_output(ns("table_tidy"))
  })

  output$table_tidy <- DT::renderDT({
    ems_data_table(stand_data())
  })

  output$dl_tidy <- downloadHandler(
    filename = function() {
      paste0(input$file_tidy, ".csv")
    },
    content = function(file) {
      readr::write_csv(stand_data(), file)
    }
  )

  return(
    list(
      data = stand_data,
      mdl_action = reactive({
        input$mdl_action
      }),
      strict = reactive({
        input$strict
      })
    )
  )
}
