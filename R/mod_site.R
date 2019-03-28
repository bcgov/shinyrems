# Module UI

#' @title   mod_site_ui and mod_site_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_site
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_site_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tabsetPanel(
        tabPanel(title = "Site Map",
                 leaflet::leafletOutput(ns("leafletSites"))),
        tabPanel(title = "Site Table",
                 dataTableOutput(ns("tableSites")))
      )
    )
  )
}

# Module Server

#' @rdname mod_site
#' @export
#' @keywords internal

mod_site_server <- function(input, output, session){
  ns <- session$ns

  output$leafletSites <- leaflet::renderLeaflet({
    leaflet::leaflet(data = ems_site_lookup) %>%
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
      # setView(lng = centroid_lng, lat = centroid_lat, zoom = zoom) %>%

  })
}

## To be copied in the UI
# mod_site_ui("site_ui_1")

## To be copied in the server
# callModule(mod_site_server, "site_ui_1")

