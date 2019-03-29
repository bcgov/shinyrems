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

p_index <- function(data, sites){
  which(data$MONITORING_LOCATION %in% sites)
}

html_site_map <- function(parameter){
  p(HTML("These sites have <strong>", parameter, "</strong>data available.
                                        Click on map markers to add/remove from selected sites."))
}

site_parameter_html <- function(parameter){
  p(HTML("These sites have <strong>", parameter, "</strong>data available.
                                        Click on a map marker or table row to add/remove from selected sites."))
}

build_data <- function(data, emsid, param_code, dates){
  historic <- filter_historic_db(emsid = emsid, param_code = param_code,
                                 from_date = dates[1], to_date = dates[2])
  yr2 <- rems::filter_ems_data(data, emsid = emsid, param_code = param_code,
                               from_date = dates[1], to_date = dates[2])
  rems::bind_ems_data(historic, yr2)
}

ems_plot <- function(data = ems_data, parameter){
  ggplot2::ggplot(data = data, ggplot2::aes(x = COLLECTION_START, y = RESULT,
                                            group = MONITORING_LOCATION,
                                            color = MONITORING_LOCATION)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_color_discrete("Sites") +
    ggplot2::xlab("Date") +
    ggplot2::ylab(parameter) +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = 'vertical')
}

ems_leaflet <- function(data, icon){
  data$LeafLabel <- leaflet_labels(data)
  leaflet::leaflet(data = data) %>%
    leaflet::addProviderTiles("Esri.WorldImagery",
                              group = "Satelite") %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik",
                     group = "Street Map") %>%
    leaflet::addLayersControl(
      baseGroups = c("Satelite", "Street Map"),
      overlayGroups = c("All Sites", "Selected Sites"),
      options = leaflet::layersControlOptions(collapsed = FALSE),
      position = "topright") %>%
    leaflet::addMapPane("paneSites", 410) %>%
    leaflet::addMapPane("paneSelectedSites", 420) %>%
    leaflet::addAwesomeMarkers(lng = ~LONGITUDE,
                        lat  = ~LATITUDE,
                        group = 'All Sites',
                        options = leaflet::pathOptions(pane = "paneSites"),
                        layerId = ~MONITORING_LOCATION,
                        label = ~lapply(LeafLabel, HTML),
                        clusterOptions = leaflet::markerClusterOptions(showCoverageOnHover = F,
                                                              spiderfyOnMaxZoom = T),
                        icon = icon)

}

ems_leaflet_update <- function(data, icon){
  leaflet::leafletProxy('leafletSites', data = data) %>%
    leaflet::clearMarkers() %>%
    leaflet::addAwesomeMarkers(lng = ~LONGITUDE,
                               lat  = ~LATITUDE,
                               group = 'Selected Sites',
                               options = leaflet::pathOptions(pane = "paneSelectedSites"),
                               layerId = ~MONITORING_LOCATION,
                               label = ~lapply(LeafLabel, HTML),
                               icon = icon)
}

ems_marker <- function(colour){
  leaflet::makeAwesomeIcon(icon = "flag", markerColor = colour, iconColor = 'white')
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
