site_to_emsid <- function(sites){
  x <- ems_sites
  x$EMS_ID[x$MONITORING_LOCATION %in% sites]
}

parameter_to_paramcode <- function(parameter){
  x <- rems::ems_parameters
  x$PARAMETER_CODE[x$PARAMETER == parameter]
}

parameter_to_sites <- function(parameter){
  x <- ems_site_parameters
  x$MONITORING_LOCATION[which(x$PARAMETER == parameter)]
}

parameter_to_location <- function(parameter){
  x <- ems_site_parameters
  x[which(x$PARAMETER == parameter), ]
}

ems_plot <- function(data = ems_data){
  ggplot2::ggplot(data = data) +
    ggplot2::geom_line(ggplot2::aes(x = COLLECTION_START, y = RESULT,
                                    group = MONITORING_LOCATION,
                                    color = MONITORING_LOCATION)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = 'vertical')
}

ems_leaflet <- function(data){
  leaflet::leaflet(data = data) %>%
    leaflet::addProviderTiles("Esri.WorldImagery",
                              options = leaflet::providerTileOptions(opacity = 1),
                              group = "Satelite") %>%
    leaflet::addProviderTiles("Stamen.Terrain",
                              options = leaflet::providerTileOptions(opacity = 1),
                              group = "Terrain") %>%
    leaflet::addLayersControl(
      baseGroups = c("Satelite", "Terrain"),
      overlayGroups = c("Sites"),
      options = leaflet::layersControlOptions(collapsed = TRUE),
      position = "topright") %>%
    leaflet::addMapPane("paneSites", 410) %>%
    leaflet::addMarkers(lng = ~LONGITUDE,
                        lat  = ~LATITUDE,
                        group = 'Sites',
                        options = leaflet::pathOptions(pane = "paneSites"))

}

# this is copied from rems::read_historic_data, but missing code asking to update/download db
filter_historic_db <- function(emsid, parameter = NULL, param_code, from_date, to_date, cols = NULL){
  db_path <- rems:::write_db_path()
  qry <- rems:::construct_historic_sql(emsid = emsid, parameter = parameter,
                                param_code = param_code, from_date = from_date, to_date = to_date,
                                cols = cols)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))
  res <- DBI::dbGetQuery(con, qry)
  if (!is.null(res$COLLECTION_START))
    res$COLLECTION_START <- rems:::ems_posix_numeric(res$COLLECTION_START)
  if (!is.null(res$COLLECTION_END))
    res$COLLECTION_END <- rems:::ems_posix_numeric(res$COLLECTION_END)
  ret <- tibble::as_tibble(res)
  rems:::add_rems_type(ret, "historic")
}
