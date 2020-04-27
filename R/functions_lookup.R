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


get_lookup <- function(dataset) {
  lookup <- NULL
  if (dataset %in% c("2yr", "4yr")) {
    lookup <- rems::get_ems_lookup(dataset)
  }

  if (dataset == "historic") {
    lookup <- lookup_historic
  }

  if (dataset == "all") {
    lookup1 <- lookup_historic
    lookup2 <- rems::get_ems_lookup("2yr")
    lookup <- rbind(lookup1, lookup2)
  }

  if (dataset == "demo") {
    lookup <- lookup_demo
  }

  lookup
}

get_lookup_location <- function(data) {
  data %>%
    dplyr::distinct(
      .data$EMS_ID, .data$MONITORING_LOCATION,
      .data$PERMIT, .data$LATITUDE,
      .data$LONGITUDE
    )
  # dplyr::as_tibble()
}
