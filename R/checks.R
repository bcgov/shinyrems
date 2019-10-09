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

check_2yr_data <- function(){
  message("checking for most recent 2 years of data...")
  if(!rems:::._remsCache_$exists("2yr")){
    rems::get_ems_data("2yr", ask = TRUE, check_only = FALSE, dont_update = TRUE)
  }
  rems::get_ems_data("2yr", ask = FALSE, check_only = TRUE, dont_update = TRUE)
}

check_historic_data <- function(){
  message("checking for historic data...")
  rems::download_historic_data(ask = TRUE)
}

check_all_data <- function(){
  check_2yr_data()
  check_historic_data()
}




