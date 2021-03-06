# Copyright 2020 Province of British Columbia
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


ems_leaflet <- function(watershed_groups, sites, site_type) {
  id <- site_col(site_type)
  leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Basemap") %>%
    addLayersControl(
      baseGroups = c("Basemap", "Satelite"),
      overlayGroups = c(
        "Watershed Groups",
        "All Sites",
        "Selected Sites"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addPolygons(
      data = watershed_groups,
      group = "Watershed Groups",
      fillOpacity = 0.1, color = "black",
      fillColor = "black",
      weight = 1.5,
      layerId = ~WATERSHED_GROUP_NAME,
      label = ~WATERSHED_GROUP_NAME,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "black",
        bringToFront = FALSE,
        fillOpacity = 0.1,
        fillColor = "blue"
      )
    ) %>%
    leaflet::addCircleMarkers(data = sites,
                              radius = 5,
                              weight = 2,
                              fillOpacity = 0.3,
                              # icon = icon_blue,
                              lng = ~LONGITUDE,
                              lat = ~LATITUDE,
                              group = "All Sites",
                              layerId = sites[[id]],
                              label = sites[[id]])
    # addAwesomeMarkers(
    #   # clusterOptions = markerClusterOptions(),
    #   data = sites,
    #   icon = icon_blue,
    #   lng = ~LONGITUDE,
    #   lat = ~LATITUDE,
    #   group = "Sites",
    #   layerId = sites[[id]],
    #   label = sites[[id]]
    # )
}

zoom_to <- function(id, ws) {
  ws <- watershed_groups[watershed_groups$WATERSHED_GROUP_NAME == ws, ]
  leafletProxy(id) %>%
    setView(lng = ws$lng_center, lat = ws$lat_center, zoom = 8L) %>%
    removeShape("highlighted_polygon") %>%
    addPolygons(
      data = ws,
      group = "highlighted_polygon",
      fillOpacity = 0.1, color = "black",
      fillColor = "blue",
      weight = 1.5,
      layerId = ~WATERSHED_GROUP_NAME,
      label = ~WATERSHED_GROUP_NAME,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "black",
        bringToFront = FALSE,
        fillOpacity = 0.1,
        fillColor = "blue"
      )
    )
}
