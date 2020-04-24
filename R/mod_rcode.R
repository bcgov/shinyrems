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

#' @title   mod_rcode_ui and mod_rcode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_rcode
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_rcode_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput("ui_help"),
    div(id = 'codes',
        wellPanel(
          uiOutput(ns('rcode'))
    )
  ))
}

# Module Server

#' @rdname mod_rcode
#' @export
#' @keywords internal

mod_rcode_server <- function(input, output, session, data, tidy,
                             clean, outlier, results){
  ns <- session$ns

  dataset <- getShinyOption("dataset", "demo")

  output$rcode <- renderUI({
    tagList(
      rcode_head(dataset),
      br2(),
      data$rcode(),
      br2(),
      tidy$rcodetidy(),
      br2(),
      outlier$rcodeclean(),
      br2(),
      outlier$rcodeoutlier(),
      br2(),
      results$rcodeplot(),
      br2(),
      results$rcodetable()
    )
  })
}

## To be copied in the UI
# mod_rcode_ui("rcode_ui_1")

## To be copied in the server
# callModule(mod_rcode_server, "rcode_ui_1")

