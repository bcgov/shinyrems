preprocess_data <- function(data){
  data %<>% dplyr::mutate(dplyr::across(c(Year, Month, Day, Hour, Minute, Second), as.integer),
                          dplyr::across(c(Station, ResultLetter, Units, Variable), as.character),
                          dplyr::across(c(Value, DetectionLimit), as.numeric))
}

check_data_upload <- function(data){

  data <- preprocess_data(data)

  template <- readr::read_csv(system.file("extdata/ems_template.csv", package = "shinyrems"))
  names <- names(template)

  if(!all(names %in% names(data))){
    err("Uploaded data must have column names: ", err::cc_and(names))
  }

  check_values(data$Year, c(1900L, 2100L), x_name = "Column 'Year'")
  check_values(data$Month, c(1L, 12L), x_name = "Column 'Month'")
  check_values(data$Day, c(1L, 31L), x_name = "Column 'Day'")
  check_values(data$Hour, c(0L, 23L, NA), x_name = "Column 'Hour'")
  check_values(data$Minute, c(0L, 59L, NA), x_name = "Column 'Minute'")
  check_values(data$Second, c(0L, 59L, NA), x_name = "Column 'Second'")

  chk_no_missing(data$Station, x_name = "Column 'Station'")
  chk_no_missing(data$Value, x_name = "Column 'Station'")
  chk_no_missing(data$Variable, x_name = "Column 'Station'")

  return(data)
}

preprocess_data_upload <- function(data){
  return(data)
}

process_data_upload <- function(data, variable, site, date_range){
  data <- dplyr::filter(data,
                        Variable == variable,
                        Station %in% site,
                        as.Date(DateTime) >= date_range[1],
                        as.Date(DateTime) <= date_range[2])
  return(data)
}

process_dates <- function(data){
  second <- ifelse(is.na(data$Second), 0, data$Second)
  hour <- ifelse(is.na(data$Hour), 0, data$Hour)
  minute <- ifelse(is.na(data$Minute), 0, data$Minute)

  data %>%
    dplyr::mutate(DateTime = ISOdatetime(Year, Month, Day, hour, minute, second),
                  Year = NULL,
                  Month = NULL,
                  Day = NULL,
                  Hour = NULL,
                  Minute = NULL,
                  Second = NULL)

}
