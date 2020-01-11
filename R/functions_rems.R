which_lup <- function(x){
  paste(x, "lookup", sep = "_")
}

lookup_from_cache <- function(which){
  rems:::._remsCache_$get(which_lup(which))
}

lookup_cache_exists <- function(which){
  rems:::._remsCache_$exists(which_lup(which))
}

update_lookup_cache <- function(which, lookup){
  file_meta <- rems:::get_file_metadata(which)
  which <- which_lup(which)
  rems:::._remsCache_$set(which, lookup)
  rems:::set_cache_date(which = which, value = file_meta[["server_date"]])
}

# ems_lookup <- function(data){
#   data %>%
#     dplyr::group_by(dplyr::.data$EMS_ID, dplyr::.data$MONITORING_LOCATION,
#                     dplyr::.data$PERMIT, dplyr::.data$PARAMETER_CODE,
#                     dplyr::.data$PARAMETER, dplyr::.data$LONGITUDE, dplyr::.data$LATITUDE) %>%
#     dplyr::arrange(dplyr::.data$COLLECTION_START) %>%
#     dplyr::summarise(FROM_DATE = first(dplyr::.data$COLLECTION_START),
#               TO_DATE = last(dplyr::.data$COLLECTION_START)) %>%
#     dplyr::ungroup()
# }

### dtplyr version is much faster but not sure if adding dependency to rems is worth it
ems_lookup_dt <- function(data){
  dt <- dtplyr::lazy_dt(data)
  dt %>%
    dplyr::group_by(dplyr::.data$EMS_ID, dplyr::.data$MONITORING_LOCATION,
                    dplyr::.data$PERMIT, dplyr::.data$PARAMETER_CODE,
                    dplyr::.data$PARAMETER, dplyr::.data$LONGITUDE, dplyr::.data$LATITUDE) %>%
    dplyr::summarise(FROM_DATE = min(dplyr::.data$COLLECTION_START, na.rm = TRUE),
                     TO_DATE = max(dplyr::.data$COLLECTION_START, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::as_tibble()
}
