get_lookup <- function(dataset){
  switch(dataset,
         "2yr" = get_which_lookup(dataset),
         "4yr" = get_which_lookup(dataset),
         "historic" = lookup_historic,
         "demo" = lookup_demo,
         "all" = get_all_lookup())
}

get_all_lookup <- function(){
  lookup_2yr <- get_which_lookup("2yr")
  lookup_historic <- lookup_historic
  rbind(lookup_2yr, lookup_historic) %>%
    dtplyr::lazy_dt() %>%
    dplyr::distinct() %>%
    dplyr::as_tibble()
}

get_which_lookup <- function(which){
  if(lookup_cache_exists(which))
    return(lookup_from_cache(which))
  data <- ems_data(which)
  update_lookup_cache(which, ems_lookup_dt(data))
  lookup_from_cache(which)
}

get_lookup_location <- function(data){
  data %>%
    # dtplyr::lazy_dt() %>%
    dplyr::distinct(EMS_ID, MONITORING_LOCATION,
                    PERMIT, LATITUDE,
                    LONGITUDE)
    # dplyr::as_tibble()
}


