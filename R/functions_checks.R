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

check_ems_data <- function(which){
  message(glue("checking for most recent {which} dataset..."))
  # if(!rems:::._remsCache_$exists(which)){
  #   return(rems::get_ems_data(which, ask = TRUE, check_only = FALSE))
  # }
  rems::get_ems_data(which, ask = TRUE, check_only = FALSE)
}

check_historic_data <- function(){
  message("checking for historic data...")
  rems::download_historic_data(ask = TRUE)
}

check_all_data <- function(){
  check_historic_data()
  check_ems_data("2yr")
}

check_data_upload <- function(data, template){
  if(!grepl(".csv", data$name, fixed = TRUE)) {
    return("Please submit a csv file.")
  }
  data <- readr::read_csv(data$datapath)
  if("EMS_ID" %in% names(data)){
    data$EMS_ID <- as.character(data$EMS_ID)
  }
  x <- check_template(data, template)
  if(is.character(x)){
    return(x)
  }
  data
}

check_template <- function(x, template){
  x <- try(checkr::check_data(x = x,
                              values = sapply(template, function(x) x$check, USE.NAMES = FALSE),
                              nrow = c(1L,.Machine$integer.max),
                              error = TRUE, x_name = "data"), silent = TRUE)
  if(is_try_error(x))
    return(gsub("Error : |\n", "", x[1]))
  invisible(TRUE)
}


