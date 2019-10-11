modal_sitemap <- function(ns){
  modalDialog(
    tagList(
      help_text("Click a marker to add to selected sites.
          Select from dropdown or click polygon to zoom
                to watershed group."),
      uiOutput(ns("ui_wsgroup")),
      shinycssloaders::withSpinner(leaflet::leafletOutput(ns("site_map"))),
      br(),
      uiOutput(ns("ui_site_modal"))
    ),
    easyClose = TRUE,
    title = "Find sites on map",
    footer = modalButton("Done")
  )
}

run_mode_data_sidebar <- function(run_mode){
  if(run_mode == "upload")
    return(mod_data_upload_ui("data_upload_ui_1"))
  mod_data_find_ui("data_find_ui_1")
}

modal_data <- function(which, update, ns){
  x <- glue("You don't have the {which} dataset.")
  if(update)
    x <- glue("There is a newer {which} dataset available.")
  modalDialog(
    h4(glue("{x} Would you like to download it and store a local copy on your computer?")),
    button(ns("yes"), "Yes!"),
    button(ns("no"), "No thanks"),
    uiOutput(ns("download_text_ui")),
    footer = NULL
  )
}

# find_data_ui <- function(ns){
#   tagList(
#     checkboxInput(ns("check_permit"),
#                   label = "Filter by Permit Number",
#                   value = FALSE),
#     uiOutput(ns("ui_permit")),
#     tags$label("Select site(s) or"),
#     actionLink(ns("search_map"), label = "find sites on map"),
#     radioButtons(ns("site_type"), label = NULL,
#                  choices = c("Monitoring Location", "EMS ID"),
#                  selected = "Monitoring Location", inline = TRUE),
#     uiOutput(ns("ui_site")),
#     tags$label("Select Parameter(s)"),
#     radioButtons(ns("parameter_type"), label = NULL,
#                  choices = c("Parameter Name", "Parameter Code"),
#                  selected = "Parameter Name", inline = TRUE),
#     uiOutput(ns("ui_parameter")),
#     uiOutput(ns("ui_date"))
#   )
# }
#
# upload_data_ui <- function(ns){
#   tagList(
#     fileInput(ns("upload_data"),
#               buttonLabel = span(tagList(icon("upload"), "csv")),
#               label = "",
#               placeholder = "Upload your own dataset",
#               accept = c('.csv')),
#     button(ns('dl_template'), label = "Download Template"),
#     downloadButton(ns("dl_template_handler"), label = NULL,
#                    style = "visibility: hidden;")
#   )
# }


