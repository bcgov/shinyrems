# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

leaflet_labels <- function(data){
  name <- names(data)
  lapply(1:nrow(data), function(x){
    data <- data[x,]
    paste0("<strong>", name, ": </strong>", data[name], "<br>")
  })
}

html_site_map <- function(parameter){
  p(HTML("These sites have <strong>", parameter, "</strong>data available.
          Selected sites are shown in red. Hover over marker for more information.
          MONITORING_LOCATION indicates site, which can be searched in the site dropdown menu to the left.
         "))
}

ems_plot <- function(data, parameter){
  ggplot2::ggplot(data = data, ggplot2::aes_string(x = "COLLECTION_START", y = "RESULT",
                                            group = "MONITORING_LOCATION",
                                            color = "MONITORING_LOCATION")) +
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
  leaf <- leaflet::leaflet(data = data) %>%
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
  leaf <- leaflet::leafletProxy('leafletSites', data = data) %>%
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

