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

check_data <- function(which){
  cache_date <- rems::get_cache_date(which)
  if(is.infinite(cache_date))
    return("download")
  file_meta <- rems:::get_file_metadata(which)
  if(cache_date < file_meta[["server_date"]])
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
      c(check, which)},
      value = 0.5,
      message = "checking for data updates ..."))
  }
  withProgress({check <- check_data(which)
  c(check, which)},
               value = 0.5,
               message = "checking for data updates ...")
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


