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

#' Run the Shiny Application
#'
#' @param dataset A string indicating which dataset the app should use.
#' Possible choices are:
#' "demo" (loads a small demo dataset that does not require any downloading);
#' "2yr" (loads and prompts download/update of recent 2 years of data (from 2018-01-01 onwards));
#' "4yr" (loads and prompts download/update of recent 4 years of EMS data (from 2016-01-01 onwards));
#' "historic" (loads and prompts download/update of historic EMS data from 1964-01-01 to 2018-01-02);
#' "all" (loads both recent and historic data);
#' "upload" (allows user to upload their own dataset in either tidied or raw format).
#'
#'
#' @export
run_ems_app <- function(dataset = "2yr") {
  chk::chk_string(dataset)
  chk::chk_subset(dataset, c("demo", "2yr", "4yr", "historic", "all", "upload"))

  ems_data <- NULL
  if (dataset == "2yr") {
    ems_data <- check_ems_data("2yr")
  }

  if (dataset == "4yr") {
    ems_data <- check_ems_data("4yr")
  }

  if (dataset == "historic") {
    check_historic_data()
  }

  if (dataset == "all") {
    ems_data <- check_all_data()
  }

  lookup <- NULL
  lookup_location <- NULL
  if(dataset != "upload"){
    lookup <- get_lookup(dataset)
    lookup_location <- lookup %>%
      dplyr::filter(!is.na(.data$LONGITUDE)) %>%
      dplyr::filter(!is.na(.data$LATITUDE)) %>%
      get_lookup_location() %>%
      add_lookup_wsgroup()
    wsheds <- setdiff(unique(lookup_location$WATERSHED_GROUP_NAME), NA)
    watershed_groups <- watershed_groups[which(watershed_groups$WATERSHED_GROUP_NAME %in% wsheds),]
  } else {
    # if upload get historic lookup so doesnt require download
    lookup <- lookup_historic
  }

  shinyOptions(
    dataset = dataset,
    lookup = lookup,
    lookup_location = lookup_location,
    watershed_groups = watershed_groups,
    ems_data = ems_data
  )

  shiny::runApp(system.file("app", package = "shinyrems"), launch.browser = TRUE)
}
