modal_sitemap <- function(ns){
  bsplus::bs_modal(
    id = ns("modal_map"),
    # trigger = ns("search_map"),
    title = "Find sites on map",
    body = tagList(
      help_text("Click a marker to add to selected sites.
          Select from dropdown or click polygon to zoom
                to watershed group."),
      uiOutput(ns("ui_wsgroup")),
      shinycssloaders::withSpinner(leaflet::leafletOutput(ns("site_map"))),
      br(),
      uiOutput(ns("ui_site_modal"))
    ),
    footer = bsplus::bs_modal_closebutton(label = "Done")
    # footer = button(ns("done"), icon = icon(NULL), label = "Done")
  )
}

run_mode_data_sidebar <- function(run_mode){
  if(run_mode == "upload")
    return(mod_data_upload_ui("data_upload_ui_1"))
  mod_data_find_ui("data_find_ui_1")
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
