library(rems)
library(dplyr)
library(ggplot2)
library(checkr)

demo_parameters <- c("Temperature", "pH", "Phosphorous Total", "Turbidity")
demo_data_historic <- rems::read_historic_data(parameter = demo_parameters)
demo_data_2yr <- rems::get_ems_data() %>%
  rems::filter_ems_data(parameter = demo_parameters)

ems_demo_data <- rems::bind_ems_data(demo_data_historic, demo_data_2yr)

demo_sites <- ems_demo_data$EMS_ID[1:1000]
ems_demo_data %<>% filter(EMS_ID %in% demo_sites)

ems_demo_parameters <- ems_demo_data %>%
  group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE, PARAMETER) %>%
  summarise() %>%
  ungroup()

ems_demo_sites <- ems_demo_parameters %>%
  group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE) %>%
  summarise() %>%
  ungroup()

check_key(ems_demo_sites, "EMS_ID")
check_key(ems_demo_parameters, c("EMS_ID", "PARAMETER"))

library(usethis)
use_data(ems_demo_sites, overwrite = TRUE)
use_data(ems_demo_parameters, overwrite = TRUE)
use_data(ems_demo_data, overwrite = TRUE)


