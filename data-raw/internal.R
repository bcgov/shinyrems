library(rems)
library(dplyr)
library(checkr)
library(poisspatial)
library(sf)
library(rmapshaper)

data_2yr <- rems::get_ems_data(which = "2yr", dont_update = TRUE)
data_historic <- rems::read_historic_data(check_db = FALSE)

lookup <- function(data){
  data %>%
    group_by(EMS_ID, MONITORING_LOCATION, PERMIT,
             PARAMETER_CODE, PARAMETER) %>%
    arrange(COLLECTION_START) %>%
    summarise(FROM_DATE = first(COLLECTION_START),
              TO_DATE = last(COLLECTION_START)) %>%
    ungroup()
}

lookup_location <- function(data){
  data %>%
    select(EMS_ID, MONITORING_LOCATION, PERMIT, LATITUDE, LONGITUDE) %>%
    distinct() %>%
    ps_coords_to_sfc(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
}

lookup_2yr <- lookup(data_2yr)
lookup_historic <- lookup(data_historic)
lookup_demo <- lookup(shinyrems::ems_demo_data)

lookup_2yr_location <- lookup_location(data_2yr)
lookup_historic_location <- lookup_location(data_historic)
lookup_demo_location <- lookup_location(shinyrems::ems_demo_data)

check_key(lookup_2yr_location, "EMS_ID")
check_key(lookup_historic_location, "EMS_ID")

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

run_modes <- c("2yr", "historic", "all", "demo", "upload")

usethis::use_data(lookup_2yr, lookup_2yr_location,
                  lookup_historic, lookup_historic_location,
                  lookup_demo, lookup_demo_location,
                  template_raw, template_tidy, watershed_groups,
                  run_modes, internal = TRUE, overwrite = TRUE)

