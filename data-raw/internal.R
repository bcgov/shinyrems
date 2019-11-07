library(rems)
library(dplyr)
library(data.table)
library(checkr)
library(poisspatial)
library(sf)
library(rmapshaper)

data_2yr <- rems::get_ems_data(which = "2yr", dont_update = TRUE, force = TRUE)
data_historic <- rems::read_historic_data(check_db = FALSE)

lookup <- function(data){
  dt <- data.table::data.table(data)
  lookup <- dt[, .(FROM_DATE = min(COLLECTION_START, na.rm = TRUE),
                   TO_DATE = max(COLLECTION_START, na.rm = TRUE)),
               .(EMS_ID, MONITORING_LOCATION, PERMIT,
                 PARAMETER_CODE, PARAMETER, LONGITUDE, LATITUDE)]
  as.data.frame(lookup)
}

lookup_2yr <- lookup(data_2yr)
lookup_historic <- lookup(data_historic)
lookup_demo <- lookup(shinyrems::ems_demo_data)

watershed_groups <- st_read("~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", layer = "FWA_WATERSHED_GROUPS_POLY")
watershed_groups <- ms_simplify(watershed_groups, keep = 0.01) %>%
  st_transform(4326)
cent <- st_coordinates(st_centroid(watershed_groups))
watershed_groups$lng_center <- cent[,1]
watershed_groups$lat_center <- cent[,2]

watershed_groups <- watershed_groups %>% mutate_if(is.factor, as.character)

template_tidy <- list(EMS_ID = list(example = "0124784",
                                    type = "character",
                                    description = "EMS ID of measurement.",
                                    check = c("")),
                      Station = list(example = "RUSSELL SLOUGH",
                                     type = "character",
                                     description = "Station of measurement.",
                                     check = c("")),
                      DateTime = list(example = "2018-02-21 13:30:00",
                                      type = "character",
                                      description = "Date and time of measurement in y-m-d h:m:s format.",
                                      check = c(Sys.time())),
                      Variable = list(example = "Aluminum Dissolved",
                                      type = "character",
                                      description = "Variable.",
                                      check = c("")),
                      Code = list(example = "AL-D",
                                  type = "character",
                                  description = "Parameter code.",
                                  check = c("")),
                      Value = list(example = 1.1,
                                   type = "numeric",
                                   description = "Value of measurement.",
                                   check = c(0)),
                      Units = list(example = "degC",
                                   type = "character",
                                   description = "Units of measurement.",
                                   check = c("")),
                      DetectionLimit = list(example = 0.01,
                                            type = "number",
                                            description = "Detection limit.",
                                            check = c(1, NA)),
                      ResultLetter = list(example = "<",
                                          type = "character",
                                          description = "Relationship to detection limit.",
                                          check = c("<", ">", "M", NA)),
                      SAMPLE_STATE = list(example = "Fresh Water",
                                          type = "character",
                                          description = "Sample state.",
                                          check = c("", NA)),
                      SAMPLE_CLASS = list(example = "Regular",
                                          type = "character",
                                          description = "Sample class.",
                                          check = c("", NA)),
                      SAMPLE_DESCRIPTOR = list(example = "General",
                                          type = "character",
                                          description = "Sample descriptor.",
                                          check = c("", NA)),
                      LOCATION_TYPE = list(example = "RIVER,STREAM OR CREEK",
                                          type = "character",
                                          description = "Location type.",
                                          check = c("", NA)),
                      UPPER_DEPTH = list(example = 0.1,
                                           type = "numeric",
                                           description = "Upper Depth.",
                                           check = c(1, NA)),
                      LOWER_DEPTH = list(example = 0.1,
                                         type = "numeric",
                                         description = "Lower Depth.",
                                         check = c(1, NA)))

raw_names <- c(EMS_ID = "EMS_ID", Station = "MONITORING_LOCATION",
               DateTime = "COLLECTION_START", Variable = "PARAMETER",
               Code = "PARAMETER_CODE", Value =  "RESULT",
               Units = "UNIT", DetectionLimit = "METHOD_DETECTION_LIMIT",
               ResultLetter = "RESULT_LETTER")

datasets <- c("2yr", "historic", "all", "demo", "upload")

ems_reference_tables <- list("Collection Methods" = rems::ems_coll_methods,
                             "Location Samples" = rems::ems_location_samples,
                             "Parameters" = rems::ems_parameters,
                             "Sample Classes" = rems::ems_sample_classes,
                             "Species" = rems::ems_species,
                             "Units" = rems::ems_units)

usethis::use_data(lookup_2yr, lookup_historic, lookup_demo,
                  template_raw, template_tidy, watershed_groups,
                  datasets, ems_reference_tables, raw_names,
                  internal = TRUE, overwrite = TRUE)

