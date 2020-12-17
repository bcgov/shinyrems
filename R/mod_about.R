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

#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
mod_about_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      1,
      HTML("")
    ),
    column(
      11,
      h4(paste("Welcome!")),
      br(),
      h5(
        "See the", a("User Guide", href = "https://bcgov.github.io/shinyrems/articles/shinyrems.html"),
        "for a more detailed explanation of app usage."
      ),
      h5(
        "The app is an interface to the rems and wqbc R packages. For more information, see the ",
        a("rems", href = "https://github.com/bcgov/rems"), "and",
        a("wqbc", href = "https://github.com/bcgov/wqbc"), "GitHub pages."
      ),
      actionLink(ns("info_citation"), "Citation info"),
      shinyjs::hidden(div(
        id = ns("div_citation"),
        h6(HTML(paste(
          "To cite package 'rems' in publications use:<br><br>
                                    Andy Teucher (2019). rems: Get Data From British Columbia's Environmental Monitoring System. R
                                    package version 0.4.2 <br><br>",
          "To cite app: <br><br>
                                    Seb Dalgarno (2018). shinyrems: An app for download and visualization of ems data.
                                    https://poissonconsulting.shinyapps.io/ypr/<br><br>
                                    "
        )))
      )),
      hr(),
      h6("Developed by Poisson Consulting.")
    ),
    tags$footer(actionLink(
      inputId = "poisson",
      label = img(
        src = "https://www.poissonconsulting.ca/assets/logos/poisson.png",
        height = 177 / 5,
        width = 739 / 5,
        onclick = "window.open('http://www.poissonconsulting.ca', '_blank')"
      )
    ),
    align = "center",
    style = "position: relative;
                bottom:1;
                width:100%;
                height:50px; /* Height of the footer */
                color: #2f4f4f;
                padding: 10px;
                background-color: white;
                z-index: -1000;
                font-size: 12px"
    )
  )
}

# Module Server

#' @rdname mod_about
#' @keywords internal

mod_about_server <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$info_citation, {
    shinyjs::toggle("div_citation", anim = TRUE, animType = "slide", time = 0.2)
  })
}
