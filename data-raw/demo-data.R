library(rems)
library(usethis)

demo_parameters <- c("Temperature", "Turbidity")

demo_data_historic <- rems::read_historic_data(parameter = demo_parameters)
demo_data_2yr <- rems::get_ems_data() %>%
  rems::filter_ems_data(parameter = demo_parameters)

ems_demo_data <- rems::bind_ems_data(demo_data_historic, demo_data_2yr)
ems_demo_data <- ems_demo_data[which(ems_demo_data$MONITORING_LOCATION %in% ems_demo_data$MONITORING_LOCATION[1:500]),]

use_data(ems_demo_data, overwrite = TRUE)


