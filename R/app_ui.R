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

app_ui <- function() {
  run_mode <- "2yr"

  tagList(
    shinyjs::useShinyjs(),
    css_navbar(text_selected_color = "#5bc0de"),
    # css_hide_errors(),
    navbarPage(title =  "EMS Database", selected = '1. Get data',
               tabPanel(title = "1. Get data",
                        br(),
                        mod_data_ui("data_ui_1")),
               tabPanel(title = "2. Standardise data",
                        br(),
                        mod_standardise_ui("standardise_ui_1")),
               tabPanel(title = "3. Clean data",
                        br(),
                        mod_clean_ui("clean_ui_1")),
               tabPanel(title = "4. Guidelines",
                        br(),
                        mod_results_ui("results_ui_1")),
               tabPanel(title = 'Reference Tables',
                        br(),
                        mod_reference_ui("reference_ui_1")),
               tabPanel(title = 'About',
                        br(),
                        mod_about_ui("about_ui_1")
                        ))
  )
}
