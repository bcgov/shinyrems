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

app_server <- function(input, output, session) {

  shinyhelper::observe_helpers(help_dir = system.file("helpfiles", package = "shinyrems"))

  dataset <- getShinyOption("dataset", "demo")

  showModal(session = session, disclaimer_modal(dataset))

  callModule(mod_about_server, "about_ui_1")

  callModule(mod_reference_server, "reference_ui_1")

  if(dataset == "upload"){
    data <- callModule(mod_upload_server, "upload_ui_1")
  } else {
    data <- callModule(mod_data_server, "data_ui_1")
  }

  tidy <- callModule(mod_tidy_server, "tidy_ui_1", data)

  clean <- callModule(mod_clean_server, "clean_ui_1", tidy)

  outlier <- callModule(mod_outlier_server, "outlier_ui_1", clean, tidy)

  callModule(mod_results_server, "results_ui_1", data, tidy, clean, outlier)

}
