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
    rems::get_ems_data("2yr", ask = TRUE, check_only = FALSE, dont_update = FALSE)
  }
  rems::get_ems_data("2yr", ask = FALSE, check_only = TRUE, dont_update = TRUE)
}

check_data <- function(which){
  cache_date <- rems::get_cache_date(which)
  if(is.infinite(cache_date))
    return("download")
  file_meta <- rems:::get_file_metadata(which)
  if(cache_date >= file_meta[["server_date"]])
    return("update")
  "done"
}

check_data_progress <- function(which){
  if(which == "all"){
    return(withProgress({
      check <- check_data("2yr")
      which <- "2yr"
      if(check == "done"){
        check <- check_data("historic")
        which <- "historic"
      }
      list(check = check, which = which)},
      value = 0.5,
      message = "checking for data updates ..."))
  }
  withProgress({check <- check_data(which)
  list(check = check, which = which)},
               value = 0.5,
               message = "checking for data updates ...")
}




