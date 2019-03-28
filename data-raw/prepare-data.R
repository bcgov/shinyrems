library(rems)
library(dplyr)
library(ggplot2)
library(checkr)

db_path <- rems:::write_db_path()
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
on.exit(DBI::dbDisconnect(con))
site_parameter <- con %>%
  tbl("historic") %>%
  # head(5000) %>%
  group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE, PARAMETER) %>%
  summarise() %>%
  ungroup() %>%
  collect()

ems_sites <- site_parameter %>%
  group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE) %>%
  summarise() %>%
  ungroup()

ems_site_parameters <- site_parameter

check_key(ems_sites, "EMS_ID")
# there are two EMS_ID s for St. Mary's Lake
# check_key(ems_site_lookup, "MONITORING_LOCATION")
check_key(ems_site_parameters, c("EMS_ID", "PARAMETER"))

library(usethis)
use_data(ems_sites, overwrite = TRUE)
use_data(ems_site_parameters, overwrite = TRUE)

