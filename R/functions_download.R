
download_data <- function(which, session, id){
  switch(which,
         "2yr" = update_cache2("2yr", session, id),
         "historic" = rems::download_historic_data(force = TRUE, ask = FALSE))
}

update_cache2 <- function (which, session, id, n = Inf, cols = "wq") {
  file_meta <- rems:::get_file_metadata(which)
  url <- paste0(rems:::base_url(), file_meta[["filename"]])
  message("Downloading latest '", which, "' EMS data from BC Data Catalogue (url: ",
          url, ")")
  csv_file <- download_ems_data2(url, session, id)
  data_obj <- rems:::read_ems_data(csv_file, n = n, cols = NULL)
  message("Caching data on disk...")
  rems:::._remsCache_$set(which, data_obj)
  rems:::set_cache_date(which = which, value = file_meta[["server_date"]])
  message("Loading data...")
  rems:::add_rems_type(data_obj[, cols], which)
}

download_ems_data2 <- function(url, session, id){
    ext <- stringr::str_extract(url, "\\.(csv|zip)$")
    tfile <- tempfile(fileext = ext)
    res <- httr::GET(url, httr::write_disk(tfile), shinyhttr::progress(session, id))
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
