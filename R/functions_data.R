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
         "demo" = lookup_demo_location)
}

site_col <- function(site_type){
  if(site_type == "EMS ID")
    return("EMS_ID")
  "MONITORING_LOCATION"
}

param_col <- function(param_type){
  if(param_type == "Parameter Code")
    return("PARAMETER_CODE")
  "PARAMETER"
}

permit_sites <- function(permits, lookup, site_type){
  x <- site_col(site_type)
  if(!is.null(permits) && permits != ""){
    return(sort(unique(lookup[[x]][which(lookup$PERMIT %in% permits)])))
  }
  sort(unique(lookup[[x]]))
}

site_parameters <- function(sites, lookup, site_type, param_type){
  x <- site_col(site_type)
  y <- param_col(param_type)
  unique(lookup[[y]][lookup[[x]] %in% sites])
}

permits <- function(lookup){
  sort(setdiff(unique(lookup$PERMIT), NA_character_))
}

date_range <- function(sites, parameters, lookup, site_type, param_type){
  x <- site_col(site_type)
  y <- param_col(param_type)
  data <- lookup[lookup[[x]] %in% sites & lookup[[y]] %in% parameters,]
  as.Date(c(min(data$FROM_DATE, na.rm = TRUE), max(data$TO_DATE, na.rm = TRUE)))
}

translate_site <- function(x, lookup, site_type){
  col <- site_col(site_type)
  lookup$EMS_ID[lookup[[col]] %in% x]
}

translate_parameter <- function(x, lookup, param_type){
  col <- param_col(param_type)
  lookup$PARAMETER_CODE[lookup[[col]] %in% x]
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

ems_data_which <- function(which){
  rems::get_ems_data(which = which,
                     dont_update = TRUE, force = TRUE)
}

ems_data <- function(dataset, ...){
  switch(dataset,
         "demo" = filter_2yr_data(x = shinyrems::ems_demo_data, ...),
         "2yr" = filter_2yr_data(x = ems_data_which("2yr"), ...),
         "historic" = filter_historic_data(...),
         combine_data(x = ems_data_which("2yr"), ...))
}

ems_data_progress <- function(dataset, parameter,
                              site, from_date, to_date,
                              site_type, param_type, lookup){

  parameter <- translate_parameter(parameter, lookup, param_type)
  site <- translate_site(site, lookup, site_type)
                        ems_data(dataset = dataset,
                                 param_code = parameter,
                                 emsid = site,
                                 from_date = from_date,
                                 to_date = to_date)
}

ems_tidy <- function(data, mdl_action, data_type, dataset, include_depth = TRUE){
  if(dataset == "upload" && data_type == "tidy"){
    data <- tidy_names_to_raw(data)
  }
  cols <- character(0)
  if(include_depth){
    cols <- c("UPPER_DEPTH", "LOWER_DEPTH")
  }
  x <- try({
  wqbc::tidy_ems_data(data, mdl_action = mdl_action,
                      cols = cols)
  }, silent = TRUE)
  if(is_try_error(x)) return(empty_tidy)
  x
}

all_depth_na <- function(data){
  all(is.na(data$LOWER_DEPTH)) && all(is.na(data$UPPER_DEPTH))
}

maxcv <- function(max_cv){
  if(is.na(max_cv))
    return(Inf)
  max_cv
}

ems_aggregate <- function(data, by, remove_blanks, max_cv, FUN){
  x <- try({
    data <- clean_wqdata2(data, by = by, max_cv = max_cv,
                remove_blanks = remove_blanks, FUN = FUN)
    first <- c("Variable", "Date", by, "Value", "Units")
    last <- setdiff(names(data), c(first, "Outlier"))
    data[, c(first, last)]
    }, silent = TRUE)
  if(is_try_error(x)) return(empty_clean)
  x
}

ems_outlier <- function(x, by = NULL, max_cv = Inf, sds = 10, ignore_undetected = TRUE,
                        large_only = TRUE, remove_blanks = FALSE,
                        FUN = "mean"){
  x <- try({
  clean_wqdata2(x = x,
                by = by,
                max_cv = max_cv,
                sds = sds,
                ignore_undetected = ignore_undetected,
                large_only = large_only,
                remove_blanks = remove_blanks,
                FUN = FUN)})
    if(is_try_error(x)) return(NULL)
    x
}

ems_standardize <- function(data, strict){
  x <- try({
  wqbc::standardize_wqdata(data, strict)
  }, silent = TRUE)
  if(is_try_error(x)) return(empty_standard)
  x
}

add_outlier_brush <- function(data, brush){
  x <- brushedPoints(data, brush, allRows = TRUE)
  x$Outlier[x$selected_] <- TRUE
  x$selected_ <- NULL
  x
}

# add_outlier_table <- function(data, rows){
#   data$Outlier[rows] <- TRUE
#   data
# }

tidy_names_to_raw <- function(x, names = raw_names){
  tmp <- sapply(names(x), function(y){
    if(!(y %in% names(raw_names))) return(y)
    raw_names[which(y == names(raw_names))] %>% setNames(NULL)
  }, USE.NAMES = FALSE)
  setNames(x, tmp)
}



