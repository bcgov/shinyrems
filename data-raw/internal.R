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


library(rems)
library(dplyr)
library(data.table)
library(checkr)
library(poisspatial)
library(sf)
library(rmapshaper)

data_historic <- rems::read_historic_data(check_db = FALSE)

lookup <- function(data) {
  dt <- data.table::data.table(data)
  lookup <- dt[
    , .(
      FROM_DATE = min(COLLECTION_START, na.rm = TRUE),
      TO_DATE = max(COLLECTION_START, na.rm = TRUE)
    ),
    .(
      EMS_ID, MONITORING_LOCATION, PERMIT,
      PARAMETER_CODE, PARAMETER, LONGITUDE, LATITUDE
    )
  ]
  df <- as.data.frame(lookup)
  df$MONITORING_LOCATION <- iconv(df$MONITORING_LOCATION,
    from = "UTF-8", to = "ASCII//TRANSLIT"
  )
  df$PARAMETER <- iconv(df$PARAMETER,
    from = "UTF-8", to = "ASCII//TRANSLIT"
  )
  df
}

lookup_historic <- lookup(data_historic)
lookup_demo <- lookup(shinyrems::ems_demo_data)

watershed_groups <- st_read("~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", layer = "FWA_WATERSHED_GROUPS_POLY")
watershed_groups <- ms_simplify(watershed_groups, keep = 0.01) %>%
  st_transform(4326)
cent <- st_coordinates(st_centroid(watershed_groups))
watershed_groups$lng_center <- cent[, 1]
watershed_groups$lat_center <- cent[, 2]

watershed_groups <- watershed_groups %>% mutate_if(is.factor, as.character)

template_tidy <- list(
  EMS_ID = list(
    example = "0124784",
    type = "character",
    description = "EMS ID of measurement.",
    check = c("")
  ),
  Station = list(
    example = "RUSSELL SLOUGH",
    type = "character",
    description = "Station of measurement.",
    check = c("")
  ),
  DateTime = list(
    example = "2018-02-21 13:30:00",
    type = "character",
    description = "Date and time of measurement in y-m-d h:m:s format.",
    check = c(Sys.time())
  ),
  Variable = list(
    example = "Aluminum Dissolved",
    type = "character",
    description = "Variable.",
    check = c("")
  ),
  Value = list(
    example = 1.1,
    type = "numeric",
    description = "Value of measurement.",
    check = c(0)
  ),
  Units = list(
    example = "degC",
    type = "character",
    description = "Units of measurement.",
    check = c("")
  ),
  DetectionLimit = list(
    example = 0.01,
    type = "number",
    description = "Detection limit.",
    check = c(1, NA)
  ),
  ResultLetter = list(
    example = "<",
    type = "character",
    description = "Relationship to detection limit.",
    check = c("<", ">", "M", NA)
  )
)

raw_names <- c(
  EMS_ID = "EMS_ID", Station = "MONITORING_LOCATION",
  DateTime = "COLLECTION_START", Variable = "PARAMETER",
  Code = "PARAMETER_CODE", Value = "RESULT",
  Units = "UNIT", DetectionLimit = "METHOD_DETECTION_LIMIT",
  ResultLetter = "RESULT_LETTER"
)

template_to_df <- function(template) {
  x <- data.frame(variable = c("example", "type", "description"))
  for (i in names(template)) {
    x[x$variable == "example", i] <- template[[i]][["example"]]
    x[x$variable == "type", i] <- template[[i]][["type"]]
    x[x$variable == "description", i] <- template[[i]][["description"]]
  }
  x <- x[, -1]
  dplyr::as_tibble(x)
}

template_tidy_df <- template_to_df(template_tidy)

datasets <- c("2yr", "4yr", "historic", "all", "demo", "upload")

empty_df <- function(data) {
  setNames(data.frame(matrix(
    ncol = length(names(data)),
    nrow = 0
  )), names(data))
}

empty_raw <- empty_df(data_historic)

tidy <- rems::get_ems_data(dont_update = TRUE) %>%
  rems::filter_ems_data(emsid = c("0121580"), parameter = c("pH")) %>%
  wqbc::tidy_ems_data()

standard <- tidy %>%
  wqbc::standardize_wqdata()

clean <- standard %>%
  wqbc::clean_wqdata()

empty_tidy <- empty_df(tidy)
empty_standard <- empty_df(standard)
empty_clean <- empty_df(clean %>% select(-Outlier))
empty_outlier <- empty_df(clean)

ems_reference_tables <- list(
  "Limits" = wqbc::limits,
  "Collection Methods" = rems::ems_coll_methods,
  "Location Samples - Sample State" = rems::ems_location_samples,
  "Parameters" = rems::ems_parameters,
  "Sample Classes" = rems::ems_sample_classes,
  "Species" = rems::ems_species,
  "Units" = rems::ems_units
)

## code to prepare `limits` dataset goes here
# limits2 <- bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
# limits_new <- dplyr::mutate(limits2,
#                             Condition = dplyr::if_else(Condition == "",
#                                                        NA_character_, Condition)) %>%
#   dplyr::filter(Use == "Aquatic Life - Freshwater",
#                 Direction == "Upper Limit",
#                 Media == "Water") %>%
#   dplyr::mutate(Use = "Freshwater Life") %>%
#   dplyr::mutate(Term = dplyr::if_else(Days == 30, "Long", "Short")) %>%
#   ### cases with multiple EMS_CODE
#   dplyr::group_by(Variable) %>%
#   dplyr::mutate(EMS_Code2 = dplyr::first(EMS_Code)) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(EMS_Code2 == EMS_Code) %>%
#   ### remove cases with ConditionNotes
#   dplyr::filter(is.na(ConditionNotes)) %>%
#   dplyr::mutate(Variable = paste(Variable, Component),
#                 UpperLimit = Limit) %>%
#   dplyr::select(Variable, Use, Term,
#                 Condition, UpperLimit, Units,
#                 Statistic, EMS_Code)
#
# ### remove mercury duplicate if hasnt been resolved yet on bcdata
# mercury_index <- limits_new$Variable == "Mercury Total" & limits_new$UpperLimit == 0.02
#
# if(is.na(limits_new$Condition[mercury_index])){
#   limits_new <- limits_new[!mercury_index,]
# }
#
# ### ensure that no duplicates
# expect_true(all(limits_new %>%
#                   dplyr::group_by(Variable, Use, Term, Condition) %>%
#                   dplyr::mutate(n = dplyr::n()) %>%
#                   dplyr::ungroup() %>%
#                   dplyr::pull(n)))
#
# limits <- limits_new

usethis::use_data(lookup_historic, lookup_demo,
  template_tidy, template_tidy_df,
  watershed_groups, empty_raw,
  empty_tidy, empty_standard, empty_clean, empty_outlier,
  datasets, ems_reference_tables, raw_names,
  internal = TRUE, overwrite = TRUE
)
