# Copyright 2019 Province of British Columbia
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

#' @title   mod_reference_ui and mod_reference_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_reference
#'
#' @keywords internal
mod_reference_ui <- function(id){
  ns <- NS(id)
  fillPage(
    fillRow(height = "90%", width = 350, flex = c(2, 1),
      selectInput(ns("selectTable"), label = NULL,
                  choices = c("Parameters", "Location Samples - Sample State",
                              "Collection Methods", "Sample Classes",
                              "Species", "Units"),
                  selected = "Parameters"),
      dl_button(ns("download"), label = "Download")),
      br3(),
      ems_table_output(ns("table")
    )
  )
}

# Module Server

#' @rdname mod_reference
#' @keywords internal

mod_reference_server <- function(input, output, session){
  ns <- session$ns

  table <- reactive({
    ems_reference_tables[[input$selectTable]]
  })

  output$table <- DT::renderDT({
    ems_data_table(table())
    })

  output$download <- downloadHandler(
    filename = function(){
      x <- gsub(" ", "", input$selectTable)
      glue::glue("ems_{x}.csv")
    },
    content = function(file) {
      readr::write_csv(table(), file)
    })
}
