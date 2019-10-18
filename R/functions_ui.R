site_map <- function(ns){
  tagList(
    help_text("Click a marker to add to selected sites.
          Select from dropdown or click polygon to zoom
                to watershed group."),
    uiOutput(ns("ui_wsgroup")),
    shinycssloaders::withSpinner(leaflet::leafletOutput(ns("leaf"))),
    br(),
    uiOutput(ns("ui_map_site"))
  )
}

data_download_modal <- function(check, which, ns){
  x <- glue("There is a newer version of the {which} dataset available.")
  id <- "no_update"
  if(check == "download"){
    x <- glue("You don't have the {which} dataset.")
    id <- "no_download"
  }

  modalDialog(
  tagList(
    p(glue("{x} Would you like to download it?")),
    button(ns("yes_download"), "Yes!", icon = icon(NULL)),
    button(ns(id), "No", icon = icon(NULL)),
    br2(),
    textOutput(ns("download_text")),
    shinyWidgets::progressBar(
      id = ns("download_progress"),
      value = 0,
      title = "",
      display_pct = TRUE
    )
  ), title = NULL, footer = NULL
  )
}

error_modal <- function(x){
  modalDialog(title = "Please fix the following error and resubmit:",
              footer = modalButton(label = "Got it"),
    tagList(
      p(x)
    )
  )}
