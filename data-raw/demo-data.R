library(rems)
library(usethis)

demo_parameters <- c("Temperature", "pH", "Turbidity")
demo_data_historic <- rems::read_historic_data(parameter = demo_parameters)
demo_data_2yr <- rems::get_ems_data() %>%
  rems::filter_ems_data(parameter = demo_parameters)

ems_demo_data <- rems::bind_ems_data(demo_data_historic, demo_data_2yr)

use_data(ems_demo_data, overwrite = TRUE)


