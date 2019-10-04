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

#' Run the Shiny Application
#'
#' @param run_mode A string indicating which dataset the app should use.
#' Possible choices are:
#' "demo" (loads a small demo dataset that does not require any downloading);
#' "2yr" (loads and prompts download/update of recent EMS data from 2018-01-01 onwards);
#' "historic" (loads and prompts download/update of historic EMS data from 1964-01-01 to 2018-01-02);
#' "all" (loads both recent and historic data).
#'
#' @export
run_app <- function(run_mode = "2yr") {
  checkr::check_vector(run_mode, c("demo", "2yr", "historic", "all"))

  switch(run_mode,
         "2yr" = check_2yr_data(),
         "historic" = check_historic_data(),
         "all" = check_all_data())

  shinyOptions(run_mode = run_mode)
  shiny::runApp(system.file("app", package = "shinyrems"), launch.browser = TRUE)
}
