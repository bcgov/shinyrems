ems_leaflet <- function(watershed_groups, sites, site_type){

  id <- site_col(site_type)
  leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Basemap") %>%
    addLayersControl(
      baseGroups = c("Basemap", "Satelite"),
      overlayGroups = c("Watershed Groups",
                        "Sites"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addPolygons(data = watershed_groups,
                group = "Watershed Groups",
                fillOpacity = 0.1, color = "black",
                fillColor = "black",
                weight = 1.5,
                layerId = ~WATERSHED_GROUP_NAME,
                label = ~WATERSHED_GROUP_NAME,
                highlightOptions = highlightOptions(weight = 2,
                                                    color = "black",
                                                    bringToFront = FALSE,
                                                    fillOpacity = 0.3,
                                                    fillColor = "blue")) %>%
    addAwesomeMarkers(data = sites,
                      icon = icon_blue,
                      lng = ~LONGITUDE,
                      lat = ~LATITUDE,
                      group = "Sites",
                      layerId = sites[[id]],
                      label = sites[[id]])

  # if(is.null(selected_sites)) return(leaf)
  # selected <- sites[sites[[id]] %in% selected_sites,]
  #
  #  leaf %>%
  #   addAwesomeMarkers(data = selected,
  #                     icon = icon_red,
  #                     lng = ~LONGITUDE,
  #                     lat = ~LATITUDE,
  #                     group = "Sites",
  #                     layerId = selected[[id]],
  #                     label = selected[[id]])

}

zoom_to <- function(id, ws){
  ws <- watershed_groups[watershed_groups$WATERSHED_GROUP_NAME == ws,]
  leafletProxy(id) %>%
    setView(lng = ws$lng_center, lat = ws$lat_center, zoom = 8L)
}

icon_blue <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
icon_red <- makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'black')


