# Copyright 2019 Province of British Columbia
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
library(usethis)

demo_parameters <- c("Temperature", "Turbidity")

demo_data_historic <- rems::read_historic_data(parameter = demo_parameters)
demo_data_2yr <- rems::get_ems_data() %>%
  rems::filter_ems_data(parameter = demo_parameters)

ems_demo_data <- rems::bind_ems_data(demo_data_historic, demo_data_2yr)
ems_demo_data <- ems_demo_data[which(ems_demo_data$MONITORING_LOCATION %in% ems_demo_data$MONITORING_LOCATION[1:500]),]

ems_reference_tables <- list("Collection Methods" = rems::ems_coll_methods,
                         "Location Samples" = rems::ems_location_samples,
                         "Parameters" = rems::ems_parameters,
                         "Sample Classes" = rems::ems_sample_classes,
                         "Species" = rems::ems_species,
                         "Units" = rems::ems_units)

use_data(ems_demo_data, overwrite = TRUE)
use_data(ems_reference_tables, overwrite = TRUE, internal = TRUE)

