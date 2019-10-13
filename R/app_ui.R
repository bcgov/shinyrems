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
                        sidebarLayout(
                          sidebarPanel(class = 'sidebar',
                                       mod_dataset_ui("dataset_ui_1"),
                                       uiOutput("data_sidebar_ui")),
                          mainPanel(mod_data_view_ui("data_view_ui_1")))),
               tabPanel(title = "2. Refine data",
                        br(),
                        sidebarLayout(
                          sidebarPanel(class = "sidebar",
                                       mod_refine_sidebar_ui("refine_sidebar_ui_1")),
                          mainPanel(mod_refine_view_ui("refine_view_ui_1"))
                        )),
               tabPanel(title = 'Reference Tables',
                        br(),
                        mod_reference_ui("reference_ui_1")),
               tabPanel(title = 'About',
                        br(),
                        mod_about_ui("about_ui_1")
                        ))
  )
}
