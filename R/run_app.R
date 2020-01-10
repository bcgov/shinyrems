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
#' @param dataset A string indicating which dataset the app should use.
#' Possible choices are:
#' "demo" (loads a small demo dataset that does not require any downloading);
#' "2yr" (loads and prompts download/update of recent EMS data from 2018-01-01 onwards);
#' "historic" (loads and prompts download/update of historic EMS data from 1964-01-01 to 2018-01-02);
#' "all" (loads both recent and historic data).
#' "upload" (allows user to upload their own dataset in either tidied or raw format).
#'
#'
#' @export
run_ems_app <- function(dataset = "2yr") {

  checkr::check_vector(dataset, c("demo", "2yr", "4yr", "historic", "all", "upload"))

  if(dataset == "2yr")
    check_ems_data("2yr")

  if(dataset == "4yr")
    check_ems_data("4yr")

  if(dataset == "historic")
    check_historic_data()

  if(dataset == "all")
    check_all_data()

  shinyOptions(dataset = dataset)

  shiny::runApp(system.file("app", package = "shinyrems"), launch.browser = TRUE)
}
