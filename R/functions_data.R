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

########## ---------- lookups ---------- ##########
run_mode_lookup <- function(run_mode){
  switch(run_mode,
         "2yr" = lookup_2yr,
         "historic" = lookup_historic,
         "demo" = lookup_demo)
}

run_mode_lookup_location <- function(run_mode){
  switch(run_mode,
         "2yr" = lookup_2yr_location,
         "historic" = lookup_historic_location,
         "demo" = lookup_demo_loation)
}

permit_sites <- function(permits, lookup){
  if(!is.null(permits) && permits != ""){
    return(sort(unique(lookup$EMS_ID[which(lookup$PERMIT %in% permits)])))
  }
  sort(unique(lookup$EMS_ID))
}

monitoring_locations <- function(sites, lookup){
  unique(lookup$MONITORING_LOCATION[lookup$EMS_ID %in% sites])
}

site_parameters <- function(sites, lookup){
  unique(lookup$PARAMETER_CODE[lookup$EMS_ID %in% sites])
}

parameter_names <- function(parameters, lookup){
  unique(lookup$PARAMETER[lookup$PARAMETER_CODE %in% parameters])
}

permits <- function(lookup){
  sort(setdiff(unique(lookup$PERMIT), NA_character_))
}

date_range <- function(sites, parameters, lookup){
  data <- lookup[lookup$EMS_ID %in% sites & lookup$PARAMETER_CODE %in% parameters,]
  as.Date(c(min(data$FROM_DATE, na.rm = TRUE), max(data$TO_DATE, na.rm = TRUE)))
}

translate_sites <- function(x, lookup, site_type){
  if(is.null(x)){
    return("")
  }
  if(site_type == "EMS ID"){
    return(x)
  }
  names(x) <- monitoring_locations(x, lookup)
  x
}

translate_parameters <- function(x, lookup, site_type){
  if(is.null(x)){
    return("")
  }
  if(site_type == "Parameter Code"){
    return(x)
  }
  names(x) <- parameter_names(x, lookup)
  x
}

########## ---------- fetching data ---------- ##########
filter_historic_data <- function(..., check_db = FALSE){
  rems::read_historic_data(..., check_db = FALSE)
}

filter_2yr_data <- function(...){
  rems::filter_ems_data(...)
}

combine_data <- function(x, ...){
  rems::bind_ems_data(
    filter_historic_data(...),
    filter_2yr_data(x = x, ...)
  )
}

ems_data <- function(){
  rems::get_ems_data(dont_update = TRUE)
}

run_mode_data <- function(run_mode, ...){
  switch(run_mode,
         "demo" = filter_2yr_data(x = shinyrems::ems_demo_data, ...),
         "2yr" = filter_2yr_data(x = ems_data(), ...),
         "historic" = filter_historic_data(...),
         combine_data(x = ems_data(), ...))
}

get_run_mode_data <- function(parameter, site, from_date, to_date, run_mode){
  switch(run_mode,
         "demo" = run_mode_data(run_mode = run_mode,
                                param_code = parameter,
                                emsid = site,
                                from_date = from_date,
                                to_date = to_date),
         shiny::withProgress(message = paste("Retrieving data..."),
                             value = 0.5, {
                               run_mode_data(run_mode = run_mode,
                                             param_code = parameter,
                                             emsid = site,
                                             from_date = from_date,
                                             to_date = to_date)}))}

download_data <- function(which){
  switch(which,
         "2yr" = rems::get_ems_data("2yr", force = TRUE, ask = FALSE),
         "historic" = rems::download_historic_data(force = TRUE, ask = FALSE))
}
