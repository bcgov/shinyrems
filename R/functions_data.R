site_to_emsid <- function(sites){
  x <- ems_sites
  x$EMS_ID[x$MONITORING_LOCATION %in% sites]
}

parameter_to_paramcode <- function(parameter){
  x <- rems::ems_parameters
  x$PARAMETER_CODE[x$PARAMETER == parameter]
}

parameter_to_site <- function(data){
  data$MONITORING_LOCATION %>% unique() %>% sort()
}

parameter_to_location <- function(data){
  data %>%
    dplyr::group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()
}

# thin wrappers for rems filter functions to simplify arguments
filter_historic_data <- function(..., check_exists = FALSE){
  rems::read_historic_data(..., check_exists = FALSE)
}

filter_2yr_data <- function(...){
  rems::filter_ems_data(...)
}

combine_data <- function(x, ...){
  rems::bind_ems_data(
    filter_historic_data(...),
    filter_2yr_data(x = x, ...)
  )
}

ems_data <- function(){
  ret <- rems:::._remsCache_$get("2yr")[, rems:::wq_cols()]
  rems:::add_rems_type(ret, "2yr")
}

run_mode_data <- function(run_mode, ...){
  switch(run_mode,
         "demo" = filter_2yr_data(x = ems_demo_data, ...),
         "2yr" = filter_2yr_data(x = ems_data(), ...),
         "historic" = filter_historic_data(...),
         combine_data(x = ems_data(), ...))
}

run_mode_date_range <- function(run_mode){
  switch(run_mode,
         "demo" = as.Date(range(ems_demo_data$COLLECTION_START, na.rm = TRUE)),
         "2yr" = c(as.Date("2018-01-01"), Sys.Date()),
         "historic" = as.Date(c("1964-01-01", "2018-01-01")),
         c(as.Date("1964-01-01"), Sys.Date()))
}

historic_parameter <- function(){
  con <- rems::attach_historic_data()
  con %>%
    dplyr::select(PARAMETER) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(PARAMETER), PARAMETER != "...") %>%
    dplyr::collect() %>%
    dplyr::pull(PARAMETER) %>%
    unique()
}

con <- rems::attach_historic_data()
tmp <- con %>%
  dplyr::filter(PARAMETER == "4Cl2BenFuran-2;3;7;8") %>%
  dplyr::collect()

yr2_parameter <- function(){
  ems_data() %>%
    dplyr::filter(!is.na(PARAMETER), PARAMETER != "...") %>%
    dplyr::pull(PARAMETER) %>%
    unique()
}

demo_parameter <- function(){
  c("Temperature", "pH", "Turbidity")
}

run_mode_parameter <- function(run_mode){
  switch(run_mode,
         "demo" = demo_parameter(),
         "2yr" = yr2_parameter(),
         "historic" = historic_parameter(),
         unique(rems::ems_parameters$PARAMETER))
}

get_run_mode_data <- function(parameter, run_mode){
  switch(run_mode,
         "demo" = run_mode_data(run_mode = run_mode,
                                parameter = parameter),
         shiny::withProgress(message = paste("Retrieving data and available sites for parameter:", parameter),
                             value = 0.5, {
                               run_mode_data(run_mode = run_mode,
                                             parameter = parameter)}))}

