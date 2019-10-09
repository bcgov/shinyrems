ems_leaflet <- function(watershed_groups, sites, site_type){

  id <- "MONITORING_LOCATION"
  if(site_type == "EMS ID")
    id <- "EMS_ID"

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
                fillColor = "blue",
                weight = 1.5,
                layerId = ~WATERSHED_GROUP_NAME,
                label = ~WATERSHED_GROUP_NAME,
                highlightOptions = highlightOptions(weight = 2,
                                                    color = "black",
                                                    bringToFront = FALSE,
                                                    fillOpacity = 0.3,
                                                    fillColor = "blue")) %>%
    addMarkers(data = sites,
               group = "Sites",
               layerId = ~EMS_ID,
               label = sites[[id]])
}

