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

#' @import shiny
app_ui <- function() {
  tagList(
    shinyjs::useShinyjs(),
    css_navbar(text_selected_color = "#5bc0de"),
    css_hide_errors(),
    navbarPage(title =  "EMS Database", selected = 'EMS',
               tabPanel(title = 'EMS',
                        br(),
                        mod_ems_ui("ems_ui_1")),
               tabPanel(title = 'Reference Tables',
                        br(),
                        mod_reference_ui("reference_ui_1")),
               tabPanel(title = 'About',
                        br(),
                        mod_about_ui("about_ui_1")
                        ))
  )
}
