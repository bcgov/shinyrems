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

pretty_dataset <- function(x){
  switch(x,
         "2yr" = "2 Year",
         "4yr" = "4 Year",
         "historic" = "Historic",
         "demo" = "Demo",
         "upload" = "Upload",
         "all" = "All Years")
}

########## ---------- lookups ---------- ##########
site_col <- function(site_type){
  if(site_type == "EMS ID")
    return("EMS_ID")
  "MONITORING_LOCATION"
}

permit_sites <- function(permits, lookup, site_type){
  x <- site_col(site_type)
  if(!is.null(permits) && permits != ""){
    return(sort(unique(lookup[[x]][which(lookup$PERMIT %in% permits)])))
  }
  sort(unique(lookup[[x]]))
}

site_parameters <- function(sites, lookup, site_type, param_strict){
  x <- site_col(site_type)
  if(param_strict == "in ANY of selected sites"){
    return(sort(unique(lookup[["PARAMETER"]][lookup[[x]] %in% sites])))
  }
  l <- lapply(sites, function(y){
    unique(lookup[["PARAMETER"]][lookup[[x]] %in% y])
  })
  sort(Reduce(intersect, l))
}

permits <- function(lookup){
  sort(setdiff(unique(lookup$PERMIT), NA_character_))
}

date_range <- function(sites, parameters, lookup, site_type){
  x <- site_col(site_type)
  data <- lookup[lookup[[x]] %in% sites & lookup[["PARAMETER"]] %in% parameters,]
  as.Date(c(min(data$FROM_DATE, na.rm = TRUE), max(data$TO_DATE, na.rm = TRUE)))
}

translate_site <- function(x, lookup, site_type){
  col <- site_col(site_type)
  unique(lookup$EMS_ID[lookup[[col]] %in% x])
}

code_to_parameter <- function(x, lookup){
  y <- gsub("EMS_", "", x)
  setdiff(unique(lookup$PARAMETER[lookup$PARAMETER_CODE %in% y]), NA)
}

parameter_to_code <- function(x, lookup){
  paste("EMS", setdiff(unique(lookup$PARAMETER_CODE[lookup$PARAMETER %in% x]), NA), sep = "_")
}

########## ---------- fetching data ---------- ##########
ems_data_which <- function(which){
  rems::get_ems_data(which = which,
                     dont_update = TRUE, force = TRUE)
}

ems_data <- function(dataset, emsid, parameter, from_date, to_date, data){
  switch(dataset,
         "demo" = rems::filter_ems_data(x = shinyrems::ems_demo_data,
                                        emsid = emsid,
                                        parameter = parameter,
                                        from_date = from_date,
                                        to_date = to_date),
         "2yr" = rems::filter_ems_data(x = data,
                                       emsid = emsid,
                                       parameter = parameter,
                                       from_date = from_date,
                                       to_date = to_date),
         "4yr" = rems::filter_ems_data(x = data,
                                       emsid = emsid,
                                       parameter = parameter,
                                       from_date = from_date,
                                       to_date = to_date),
         "historic" = rems::read_historic_data(emsid = emsid,
                                               parameter = parameter,
                                               from_date = from_date,
                                               to_date = to_date,
                                               check_db = FALSE),
         rems::bind_ems_data(
           rems::read_historic_data(emsid = emsid,
                                    parameter = parameter,
                                    from_date = from_date,
                                    to_date = to_date,
                                    check_db = FALSE),
           rems::filter_ems_data(x = data,
                                 emsid = emsid,
                                 parameter = parameter,
                                 from_date = from_date,
                                 to_date = to_date)
         ))
}

ems_tidy <- function(data, mdl_action, data_type, dataset, cols){
  if(dataset == "upload" && data_type == "tidy"){
    data <- tidy_names_to_raw(data)
  }
  x <- try({
  wqbc::tidy_ems_data(data, mdl_action = mdl_action,
                      cols = cols)
  }, silent = TRUE)
  if(is_try_error(x)) return(empty_tidy)
  x
}

ems_standardize <- function(data, strict){
  x <- try({
    wqbc::standardize_wqdata(data, strict)
  }, silent = TRUE)
  if(is_try_error(x)) return(empty_standard)
  x
}

ems_aggregate <- function(data, by, remove_blanks, max_cv, FUN){
  x <- try({
    data <- wqbc::clean_wqdata(data, by = by, max_cv = max_cv,
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
                        FUN = mean){
  x <- try({
    wqbc::clean_wqdata(x = x,
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

all_depth_na <- function(data){
  all(is.na(data$LOWER_DEPTH)) && all(is.na(data$UPPER_DEPTH))
}

maxcv <- function(max_cv){
  if(is.na(max_cv))
    return(Inf)
  max_cv
}

add_outlier_brush <- function(data, brush){
  x <- brushedPoints(data, brush, allRows = TRUE)
  x$Outlier[x$selected_] <- TRUE
  x$selected_ <- NULL
  x
}

tidy_names_to_raw <- function(x, names = raw_names){
  tmp <- sapply(names(x), function(y){
    if(!(y %in% names(raw_names))) return(y)
    raw_names[which(y == names(raw_names))] %>% setNames(NULL)
  }, USE.NAMES = FALSE)
  setNames(x, tmp)
}



