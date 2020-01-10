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

info <- list(mdl_action = "What to do with values below the detection limit.
Options are 'zero' (set the value to 0; the default), 'half' (set the value
to half the MDL), 'mdl' (set the value to equal to the MDL), or 'na'
(set the value to NA). Can also be set to 'none' to leave as is.",
             max_cv = "A number indicating the maximum permitted coefficient
             of variation for replicates. Leave input blank for default value
             of infinity.",
             sds = "The number of standard deviations above which a value
             is considered an outlier.")
