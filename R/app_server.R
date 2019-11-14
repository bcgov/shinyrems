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

  callModule(mod_about_server, "about_ui_1")

  callModule(mod_reference_server, "reference_ui_1")

  tidy_data <- callModule(mod_data_server, "data_ui_1")

  stand_data <- callModule(mod_standardise_server, "standardise_ui_1", tidy_data)

  params <- callModule(mod_clean_server, "clean_ui_1", stand_data)

  outlier_data <- callModule(mod_outlier_server, "outlier_ui_1", params, stand_data)

  callModule(mod_results_server, "results_ui_1", outlier_data)

}
