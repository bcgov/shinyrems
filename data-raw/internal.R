library(rems)
library(dplyr)
library(checkr)

data <- rems::get_ems_data(which = "2yr", dont_update = TRUE)

lookup_2yr <- data %>%
  group_by(EMS_ID, MONITORING_LOCATION, PERMIT,
           PARAMETER_CODE, PARAMETER) %>%
  arrange(COLLECTION_START) %>%
  summarise(FROM_DATE = first(COLLECTION_START),
            TO_DATE = last(COLLECTION_START)) %>%
  ungroup()

lookup_2yr_xy <- data %>%
  select(EMS_ID, MONITORING_LOCATION, PERMIT, LATITUDE, LONGITUDE) %>%
  distinct()

check_key(lookup_2yr_xy, "EMS_ID")

template <- list(EMS = list(example = 123,
                            type = "integer",
                            description = "EMS ID",
                            check = c(1, 124)),
                 this = list(example = "yup",
                             type = "character",
                             description = "This is a test.",
                             check = c("")))

usethis::use_data(lookup_2yr, lookup_2yr_xy, template, internal = TRUE, overwrite = TRUE)

