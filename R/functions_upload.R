preprocess_data <- function(data){
  data %<>% dplyr::mutate(dplyr::across(c(.data$Year, .data$Month, .data$Day, .data$Hour, .data$Minute, .data$Second), as.integer),
                          dplyr::across(c(.data$Station, .data$ResultLetter, .data$Units, .data$Variable), as.character),
                          dplyr::across(c(.data$Value, .data$DetectionLimit), as.numeric))
}

check_data_upload <- function(data){

  data <- preprocess_data(data)

  template <- readr::read_csv(system.file("extdata/ems_template.csv", package = "shinyrems"))
  names <- names(template)

  if(!all(names %in% names(data))){
    chk::err("Uploaded data must have column names: ",
             err::cc(names, conjunction = "and",
                     ellipsis = 1000))
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
                        .data$Variable == variable,
                        .data$Station %in% site,
                        as.Date(.data$DateTime) >= date_range[1],
                        as.Date(.data$DateTime) <= date_range[2])
  return(data)
}

process_dates <- function(data){
  data %>%
    dplyr::mutate(
      hour = dplyr::if_else(is.na(.data$Second), 0, .data$Second),
      minute = dplyr::if_else(is.na(.data$Minute), 0, .data$Minute),
      second = dplyr::if_else(is.na(.data$Second), 0, .data$Second),
      DateTime = ISOdatetime(.data$Year, .data$Month, .data$Day,
                             .data$hour, .data$minute, .data$second)) %>%
    dplyr::select(.data$Station, .data$Variable, .data$DateTime,
                  .data$Value, .data$DetectionLimit, .data$ResultLetter,
                  dplyr::everything())

  data

}
