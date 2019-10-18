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

lookup_2yr <- lookup_2yr[sample(1:nrow(lookup_2yr), 1000),]
lookup_historic <- lookup_historic[sample(1:nrow(lookup_historic), 1000),]

template_tidy <- list(EMS = list(example = 123,
                            type = "integer",
                            description = "EMS ID",
                            check = c(1, 124)),
                 tidy = list(example = "yup",
                             type = "character",
                             description = "This is a test.",
                             check = c("")))

template_raw <- list(EMS = list(example = 123,
                                 type = "integer",
                                 description = "EMS ID",
                                 check = c(1, 124)),
                      raw = list(example = "yup",
                                  type = "character",
                                  description = "This is a test.",
                                  check = c("")))

watershed_groups <- st_read("~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", layer = "FWA_WATERSHED_GROUPS_POLY")
watershed_groups <- ms_simplify(watershed_groups, keep = 0.01) %>%
  st_transform(4326)
cent <- st_coordinates(st_centroid(watershed_groups))
watershed_groups$lng_center <- cent[,1]
watershed_groups$lat_center <- cent[,2]

watershed_groups <- watershed_groups %>% mutate_if(is.factor, as.character)

datasets <- c("2yr", "historic", "all", "demo", "upload")

usethis::use_data(lookup_2yr, lookup_historic, lookup_demo,
                  template_raw, template_tidy, watershed_groups,
                  datasets, internal = TRUE, overwrite = TRUE)

