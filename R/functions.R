site_to_emsid <- function(sites){
  x <- ems_sites
  x$EMS_ID[x$MONITORING_LOCATION %in% sites]
}

parameter_to_paramcode <- function(parameter){
  x <- rems::ems_parameters
  x$PARAMETER_CODE[x$PARAMETER == parameter]
}

get_parameter_lookup <- function(run_mode){
  x <- ems_site_parameters
  if(run_mode == "demo"){
    x <- ems_demo_parameters
  }
  if(run_mode == "2yr"){
    x <- ems_site_parameters
  }
  if(run_mode == "historic"){
    x <- ems_site_parameters
  }
  x
}
parameter_to_sites <- function(parameter, run_mode){
  x <- get_parameter_lookup(run_mode = run_mode)
  x$MONITORING_LOCATION[which(x$PARAMETER == parameter)]
}

parameter_to_location <- function(parameter, run_mode){
  x <- get_parameter_lookup(run_mode = run_mode)
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

ems_plot <- function(data, parameter){
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
  leaflet::makeAwesomeIcon(icon = "flag", markerColor = colour, iconColor = 'white', library = "ion")
}

# thin wrappers for rems filter functions to simplify arguments
filter_historic_data <- function(emsid, param_code, dates){
  rems::read_historic_data(emsid = emsid, parameter = NULL, param_code = param_code,
                           from_date = dates[1], to_date = dates[2],
                           cols = NULL, check_exists = FALSE)
}

filter_2yr_data <- function(data, emsid, param_code, dates){
  rems::filter_ems_data(data, emsid = emsid, param_code = param_code,
                        from_date = dates[1], to_date = dates[2])
}

combine_data <- function(data, emsid, param_code, dates){
  rems::bind_ems_data(
    filter_historic_data(emsid = emsid, param_code = param_code, dates = dates),
    filter_2yr_data(data = data, emsid = emsid, param_code = param_code, dates = dates)
  )
}

run_mode_data <- function(run_mode, data, emsid, param_code, dates){
  switch(run_mode,
         "demo" = filter_2yr_data(data = ems_demo_data,
                                  emsid = emsid,
                                  param_code = param_code,
                                  dates = dates),
         "2yr" = filter_2yr_data(data = data,
                                emsid = emsid,
                                param_code = param_code,
                                dates = dates),
         "historic" = filter_historic_data(emsid = emsid,
                                           param_code = param_code,
                                           dates = dates),
         combine_data(data = ems_data(),
                      emsid = emsid,
                      param_code = param_code,
                      dates = dates))
}

run_mode_date_range <- function(run_mode){
  switch(run_mode,
         "demo" = as.Date(range(ems_demo_data$COLLECTION_START, na.rm = TRUE)),
         "2yr" = as.Date(c("2018-01-01", Sys.Date())),
         "historic" = as.Date(c("1964-01-01", "2018-01-01")),
         c(as.Date("1964-01-01"), Sys.Date()))
}

run_mode_parameter <- function(run_mode){
  switch(run_mode,
         "demo" = c("Temperature", "pH", "Turbidity"),
         unique(rems::ems_parameters$PARAMETER))
}

