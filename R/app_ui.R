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

app_ui <- function() {
  dataset <- getShinyOption("dataset", "demo")
  if(dataset == "upload"){
    mod_data <- mod_upload_ui("upload_ui_1")
  } else {
    mod_data <- mod_data_ui("data_ui_1")
  }
  tagList(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    # waiter::waiter_show_on_load(html = waiter_html("Fetching dataset ...")),
    css_navbar(text_selected_color = "#5bc0de"),
    css_hide_errors(),
    navbarPage(
      title = "ShinyRems", selected = "1. Data",
      tabPanel(
        title = "1. Data",
        br(),
        mod_data
      ),
      tabPanel(
        title = "2. Tidy",
        br(),
        mod_tidy_ui("tidy_ui_1")
      ),
      tabPanel(
        title = "3. Clean",
        br(),
        mod_clean_ui("clean_ui_1")
      ),
      tabPanel(
        title = "4. Outliers",
        br(),
        mod_outlier_ui("outlier_ui_1")
      ),
      tabPanel(
        title = "5. Plots/Statistics",
        br(),
        mod_results_ui("results_ui_1")
      ),
      tabPanel(
        title = "Reference Tables",
        br(),
        mod_reference_ui("reference_ui_1")
      ),
      tabPanel(
        title = "About",
        br(),
        mod_about_ui("about_ui_1")
      )
    )
  )
}
