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
#' @export
#' @importFrom shiny NS tagList
mod_reference_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tabsetPanel(
        tabPanel(title = "Parameters",
                 emsDownload(ns("dlParameters")),
                 emsTableOutput(ns("tableParameters"))),
        tabPanel(title = "Location Samples",
                 emsDownload(ns("dlSamples")),
                 emsTableOutput(ns("tableSamples"))),
        tabPanel(title = "Collection Methods",
                 emsDownload(ns("dlCollection")),
                 emsTableOutput(ns("tableCollection"))),
        tabPanel(title = "Sample Classes",
                 emsDownload(ns("dlClasses")),
                 emsTableOutput(ns("tableClasses"))),
        tabPanel(title = "Species",
                 emsDownload(ns("dlSpecies")),
                 emsTableOutput(ns("tableSpecies"))),
        tabPanel(title = "Units",
                 emsDownload(ns("dlUnits")),
                 emsTableOutput(ns("tableUnits")))
      )
    )
  )
}

# Module Server

#' @rdname mod_reference
#' @export
#' @keywords internal

mod_reference_server <- function(input, output, session){
  ns <- session$ns

  output$tableParameters <- renderDataTable({rems::ems_parameters})
  output$dlParameters <- downloadHandler(
    filename = function() "ems_parameters.csv",
    content = function(file) {
      readr::write_csv(rems::ems_parameters, file)
    })

  output$tableSamples <- renderDataTable({rems::ems_location_samples})
  output$dlSamples <- downloadHandler(
    filename = function() "ems_location_samples.csv",
    content = function(file) {
      readr::write_csv(rems::ems_location_samples, file)
    })

  output$tableCollection <- renderDataTable({rems::ems_coll_methods})
  output$dlCollection <- downloadHandler(
    filename = function() "ems_coll_methods.csv",
    content = function(file) {
      readr::write_csv(rems::ems_coll_methods, file)
    })

  output$tableClasses <- renderDataTable({rems::ems_sample_classes})
  output$dlClasses <- downloadHandler(
    filename = function() "ems_sample_classes.csv",
    content = function(file) {
      readr::write_csv(rems::ems_sample_classes, file)
    })

  output$tableSpecies <- renderDataTable({rems::ems_species})
  output$dlSpecies <- downloadHandler(
    filename = function() "ems_species.csv",
    content = function(file) {
      readr::write_csv(rems::ems_species, file)
    })

  output$tableUnits <- renderDataTable({rems::ems_units})
  output$dlUnits <- downloadHandler(
    filename = function() "ems_units.csv",
    content = function(file) {
      readr::write_csv(rems::ems_units, file)
    })
}
