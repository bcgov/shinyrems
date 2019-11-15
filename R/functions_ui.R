site_map <- function(ns){
  tagList(
    help_text("Click a marker to add to selected sites. Selected sites
    are shown in red. Select from dropdown or click polygon to zoom
                to watershed group. "),
    uiOutput(ns("ui_wsgroup")),
    shinycssloaders::withSpinner(leaflet::leafletOutput(ns("leaf"))),
    br()
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
    p(glue("{x} Would you like to download it? (this might take a while...)")),
    button(ns("yes_download"), "Yes!"),
    button(ns(id), "No"),
    br2(),
    help_output(ns("download_text")),
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

help_output <- function(id){
  div(textOutput(id),
      style = "color: grey;")
}

plot_outputs <- function(x, ns){
  tagList(
    plotOutput(ns(paste0("plot_", x))),
    br()
  )
}

rename_inputs <- function(site, ns){
  tagList(
    textInput(ns(site), label = paste("rename", site, "to"))
  )
}
