## code to prepare `limits` dataset goes here
limits_new <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
limits_new <- dplyr::mutate(limits_new, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))

limits_old <- wqbc::limits

usethis::use_data(limits_new, limits_old, internal = TRUE, overwrite = TRUE)
