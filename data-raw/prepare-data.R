library(rems)
library(dplyr)
library(ggplot2)
library(checkr)

ems <- readRDS("ems_data_2yr.rds")

ems_site_lookup <- ems %>%
  group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE) %>%
  slice(1) %>%
  ungroup()

check_key(ems_site_lookup, "EMS_ID")
check_key(ems_site_lookup, "MONITORING_LOCATION")

library(usethis)
use_data(ems_site_lookup)
