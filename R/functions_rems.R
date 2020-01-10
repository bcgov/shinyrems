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

ems_lookup <- function(data){
  data %>%
    dplyr::group_by(EMS_ID, MONITORING_LOCATION, PERMIT,
                    PARAMETER_CODE, PARAMETER, LONGITUDE, LATITUDE) %>%
    dplyr::arrange(COLLECTION_START) %>%
    dplyr::summarise(FROM_DATE = first(COLLECTION_START),
              TO_DATE = last(COLLECTION_START)) %>%
    dplyr::ungroup()
}

### dt version is much faster but not sure if adding dependency to rems is worth it
ems_lookup_dt <- function(data){
  dt <- data.table::data.table(data)
  lookup <- dt[, .(FROM_DATE = min(COLLECTION_START, na.rm = TRUE),
                   TO_DATE = max(COLLECTION_START, na.rm = TRUE)),
               .(EMS_ID, MONITORING_LOCATION, PERMIT,
                 PARAMETER_CODE, PARAMETER, LONGITUDE, LATITUDE)]
  as.data.frame(lookup)
}
