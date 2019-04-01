check_2yr_data <- function(){
  message("checking for most recent 2 years of data...")
  rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
}

check_historic_data <- function(){
  message("checking for historic data...")
  rems::download_historic_data(ask = FALSE)
}

check_all_data <- function(){
  check_2yr_data()
  check_historic_data()
}




