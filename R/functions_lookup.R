get_lookup <- function(dataset, data = NULL){
  switch(dataset,
         "2yr" = lookup(data),
         "historic" = lookup_historic,
         "demo" = lookup_demo,
         "all" = get_all_lookup(data),
         "upload" = lookup(data))
}

get_all_lookup <- function(data_2yr){
  lookup_2yr <- lookup(data_2yr)
  lookup_historic <- lookup_historic
  as.data.frame(unique(data.table::data.table(rbind(lookup_2yr, lookup_historic))))
}

lookup <- function(data){
  dt <- data.table::data.table(data)
  lookup <- dt[, .(FROM_DATE = min(COLLECTION_START, na.rm = TRUE),
                       TO_DATE = max(COLLECTION_START, na.rm = TRUE)),
     .(EMS_ID, MONITORING_LOCATION, PERMIT,
       PARAMETER_CODE, PARAMETER, LONGITUDE, LATITUDE)]
  as.data.frame(lookup)
}

get_lookup_location <- function(data){
  x <- c("EMS_ID", "MONITORING_LOCATION", "PERMIT",
         "LATITUDE", "LONGITUDE")
  data <- data.table::data.table(data)
  as.data.frame(unique(data, by = x)[, ..x])
}


