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

app_server <- function(input, output, session) {

  run_mode <- callModule(mod_dataset_server, "dataset_ui_1")

  observe({
    print(run_mode())
    if(run_mode() == "none"){
      shinyjs::hide("data_sidebar_ui")
    } else {
      shinyjs::show("data_sidebar_ui")
    }
    if(run_mode() == "upload"){
      output$data_sidebar_ui <- renderUI({
        mod_data_upload_ui("data_upload_ui_1")
      })
      get_data <- callModule(mod_data_upload_server, "data_upload_ui_1")
    } else {
      output$data_sidebar_ui <- renderUI({
        mod_data_find_ui("data_find_ui_1")
      })
      get_data <- callModule(mod_data_find_server, "data_find_ui_1", run_mode)
    }
    callModule(mod_data_view_server, "data_view_ui_1", get_data)
  })


  callModule(mod_refine_sidebar_server, "refine_sidebar_ui_1")

  callModule(mod_refine_view_server, "refine_view_ui_1")

  callModule(mod_about_server, "about_ui_1")

  callModule(mod_reference_server, "reference_ui_1")

}
