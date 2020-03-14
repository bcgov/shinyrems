get_ems_lookup <- function(which = "2yr", ask = TRUE){

  if(!(which %in% c("2yr", "4yr")))
    stop("`which` must be either '2yr' or '4yr'")

  which_lup <- paste(which, "lookup", sep = "_")
  which_exists <- rems:::._remsCache_$exists(which_lup)

  update <- FALSE
  if(!which_exists){
    update <- TRUE
  } else if(rems:::._remsCache_$exists("cache_dates")) {
    lup_cache_date <- rems:::get_cache_date(which_lup)
    data_cache_date <- rems:::get_cache_date(which)

    if (lup_cache_date < data_cache_date){
      update <- TRUE
    }
  }

  if (update) {
    if (ask) {
      rems:::stop_for_permission(paste0("rems would like to store a ", which,
                                 " data lookup table at ", rems:::rems_data_dir(), ". This is required to run the ShinyRems app. Is that okay?"))
    }

    message("Creating and caching lookup table ...")
    data <- try(rems:::._remsCache_$get(which), silent = TRUE)
    if(inherits(data, "try-error"))
      stop(which, " dataset must be cached before lookup table can be created. Run get_ems_data().")

    lookup <- make_lookup(data)
    update_lookup_cache(which = which, lookup)
  }

  lookup_from_cache(which_lup)
}

lookup_from_cache <- function(which){
  rems:::._remsCache_$get(which)
}

make_lookup <- function(x){
  x %>%
    dplyr::group_by(EMS_ID, MONITORING_LOCATION, PERMIT, PARAMETER_CODE,
                    PARAMETER, LONGITUDE, LATITUDE) %>%
    dplyr::arrange(COLLECTION_START) %>%
    dplyr::summarise(FROM_DATE = dplyr::first(COLLECTION_START),
                     TO_DATE = dplyr::last(COLLECTION_START)) %>%
    dplyr::ungroup()
}

update_lookup_cache <- function(which, data){
  file_meta <- rems:::get_file_metadata(which)
  which_lup <- paste(which, "lookup", sep = "_")
  rems:::._remsCache_$set(which_lup, data)
  rems:::set_cache_date(which = which_lup, value = file_meta[["server_date"]])
}


