library(rems)
library(dplyr)
library(checkr)
library(poisspatial)
library(sf)
library(rmapshaper)

data <- rems::get_ems_data(which = "2yr", dont_update = TRUE)

lookup_2yr <- data %>%
  group_by(EMS_ID, MONITORING_LOCATION, PERMIT,
           PARAMETER_CODE, PARAMETER) %>%
  arrange(COLLECTION_START) %>%
  summarise(FROM_DATE = first(COLLECTION_START),
            TO_DATE = last(COLLECTION_START)) %>%
  ungroup()

lookup_2yr_location <- data %>%
  select(EMS_ID, MONITORING_LOCATION, PERMIT, LATITUDE, LONGITUDE) %>%
  distinct() %>%
  ps_coords_to_sfc(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

check_key(lookup_2yr_location, "EMS_ID")

template <- list(EMS = list(example = 123,
                            type = "integer",
                            description = "EMS ID",
                            check = c(1, 124)),
                 this = list(example = "yup",
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

usethis::use_data(lookup_2yr, lookup_2yr_location, template, watershed_groups, internal = TRUE, overwrite = TRUE)

