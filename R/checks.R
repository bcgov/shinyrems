check_2yr_data <- function(){
  message("checking for most recent 2 years of data...")
  if(!rems:::._remsCache_$exists("2yr")){
    rems::get_ems_data("2yr", ask = TRUE, check_only = FALSE)
  }
  rems::get_ems_data("2yr", ask = FALSE, check_only = TRUE)
}

check_historic_data <- function(){
  message("checking for historic data...")
  rems::download_historic_data(ask = TRUE)
}

check_all_data <- function(){
  check_2yr_data()
  check_historic_data()
}




