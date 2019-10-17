get_ems_data2 <- function (which = "2yr", n = Inf, cols = "wq", force = FALSE,
                           ask = TRUE, dont_update = FALSE, check_only = FALSE,
                           shiny_progress = FALSE, shiny_session = NULL, shiny_id = NULL)
{
  which <- match.arg(which, c("2yr", "4yr"))
  which_exists <- rems:::._remsCache_$exists(which)
  cols <- if (cols == "wq") {
    rems:::wq_cols()
  }
  else if (cols == "all") {
    rems:::col_specs("names_only")
  }
  if (dont_update) {
    if (!which_exists) {
      stop("You have requested to not check for updates, but the ",
           which, " data is not currently cached.")
    }
    message("Fetching data from cache...")
    return(rems:::rems_data_from_cache(which, cols))
  }
  update <- FALSE
  if (force || !which_exists) {
    update <- TRUE
  }
  else if (rems:::._remsCache_$exists("cache_dates")) {
    cache_date <- rems::get_cache_date(which)
    file_meta <- rems:::get_file_metadata(which)
    if (cache_date < file_meta[["server_date"]]) {
      ans <- readline(paste0("Your version of ", which,
                             " is dated ", cache_date, " and there is a newer version available. Would you like to download it? (y/n)"))
      if (tolower(ans) == "y")
        update <- TRUE
    }
  }
  if (check_only)
    return(TRUE)
  if (update) {
    if (ask) {
      rems:::stop_for_permission(paste0("rems would like to store a copy of the ",
                                        which, " ems data at", rems:::rems_data_dir(), ". Is that okay?"))
    }
    return(update_cache2(which = which, n = n, cols = cols,
                         shiny_progress = shiny_progress,
                         shiny_session = shiny_session,
                         shiny_id = shiny_id))
  }
  message("Fetching data from cache...")
  rems_data_from_cache(which, cols)
}


update_cache2 <- function (which, n, cols,
                           shiny_progress, shiny_session, shiny_id) {
  file_meta <- rems:::get_file_metadata(which)
  url <- paste0(rems:::base_url(), file_meta[["filename"]])
  message("Downloading latest '", which, "' EMS data from BC Data Catalogue (url: ",
          url, ")")
  csv_file <- download_ems_data2(url, shiny_progress, shiny_session, shiny_id)
  data_obj <- rems:::read_ems_data(csv_file, n = n, cols = NULL)
  message("Caching data on disk...")
  lookup_obj <- ems_lookup(data_obj)
  rems:::._remsCache_$set(which, data_obj)
  rems:::set_cache_date(which = which, value = file_meta[["server_date"]])
  message("Caching lookup table on disk...")
  which_lookup <- paste(which, "lookup", sep = "_")
  rems:::._remsCache_$set(which_lookup, lookup_obj)
  rems:::set_cache_date(which = which_lookup, value = file_meta[["server_date"]])
  message("Loading data...")
  rems:::add_rems_type(data_obj[, cols], which)
}

download_ems_data2 <- function(url, shiny_progress, shiny_session, shiny_id){
  ext <- stringr::str_extract(url, "\\.(csv|zip)$")
  tfile <- tempfile(fileext = ext)
  if(shiny_progress){
    res <- httr::GET(url, httr::write_disk(tfile), shinyhttr::progress(shiny_session, shiny_id))
  } else {
    res <- httr::GET(url, httr::write_disk(tfile), httr::progress())
  }
  cat("\n")
  httr::stop_for_status(res)
  if (ext == ".zip") {
    exdir <- tempdir()
    zipfile <- res$request$output$path
    files_in_zip <- zip::zip_list(zipfile)$filename
    zip::unzip(zipfile, exdir = exdir)
    ret <- file.path(exdir, files_in_zip)
  }
  else if (ext == ".csv") {
    ret <- res$request$output$path
  }
  ret
}

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
