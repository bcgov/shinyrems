get_lookup <- function(dataset){
  lookup <- NULL
  if(dataset %in% c("2yr", "4yr"))
    lookup <- rems::get_ems_lookup(dataset)

  if(dataset == "historic")
    lookup <- lookup_historic

  if(dataset == "all"){
    lookup1 <- lookup_historic
    lookup2 <- rems::get_ems_lookup("2yr")
    lookup <- rbind(lookup1, lookup2)
  }

  if(dataset == "demo")
    lookup <- lookup_demo

  lookup
}

get_lookup_location <- function(data){
  data %>%
    # dtplyr::lazy_dt() %>%
    dplyr::distinct(.data$EMS_ID, .data$MONITORING_LOCATION,
                    .data$PERMIT, .data$LATITUDE,
                    .data$LONGITUDE)
    # dplyr::as_tibble()
}


