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


additional_parameters <- function(data, lookup, limits = wqbc::limits, codes = wqbc::codes) {
  data_variable <- unique(data$Variable)
  data_code <- unique(data$Code)

  # try matching limits Variable from data Variable
  if(!(unique(data$Variable) %in% limits$Variable)){
    param_code <- paste0("EMS_", data_code)
    param_code <- gsub("-", "_", param_code)
    variable <- codes$Variable[codes$Code %in% param_code]
  } else {
    variable <- data_variable
  }
  err_text <- paste0("Could not find variable '", data_variable, "'or code '", data_code, "' in limits table. See Limits table in 'Reference Tables' tab or visit the Water Quality Guideline app (https://bcgov-env.shinyapps.io/bc_wqg/)")

  if(!length(variable)){
    err(err_text)
  } else if(!(variable %in% unique(limits$Variable))){
    err(err_text)
  }
  guideline <- limits[limits$Variable == variable, ]
  ccodes <- extract_codes(c(guideline$Condition, guideline$UpperLimit))
  code_to_parameter(ccodes, lookup)
}


ems_data_parameter <- function(data, all_data, dataset, lookup,
                               from_date, to_date, # data
                               mdl_action, cols, # tidy
                               strict, # standardize
                               by, sds, ignore_undetected, large_only, # aggregate
                               remove_blanks, max_cv, FUN, ...) {
  emsid <- unique(data$EMS_ID)
  params <- additional_parameters(data, lookup)

  data <- ems_data(dataset,
    emsid = emsid, parameter = params,
    from_date = from_date, to_date = to_date, data = all_data
  )
  data <- ems_tidy(data,
    mdl_action = mdl_action,
    dataset = dataset, cols = cols
  )
  data <- ems_standardize(data, strict = strict)
  data <- ems_outlier(data,
    by = by, max_cv = max_cv, sds = sds,
    ignore_undetected = ignore_undetected, large_only = large_only,
    remove_blanks = remove_blanks, FUN = FUN
  )
  data
}
