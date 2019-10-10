# Module UI

#' @title   mod_data_upload_ui and mod_data_upload_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_upload
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      fileInput(ns("upload_data"),
                buttonLabel = span(tagList(icon("upload"), "csv")),
                label = "",
                placeholder = "Upload your own dataset",
                accept = c('.csv')),
      button(ns('dl_template'), label = "Download Template"),
      downloadButton(ns("dl_template_handler"), label = NULL,
                     style = "visibility: hidden;")
    )
  )
}

# Module Server

#' @rdname mod_data_upload
#' @export
#' @keywords internal

mod_data_upload_server <- function(input, output, session, run_mode){
  ns <- session$ns

  observeEvent(input$dl_template, {
    shinyjs::runjs(click_js(ns("dl_template_handler")))
  })

  output$dl_template_handler <- downloadHandler(
    filename = function() "ems_template.csv",
    content = function(file) {
      readr::write_csv(template_to_df(template), file)
    })

  get_data <- reactive({
    data <- input$data_upload
    if(!grepl(".csv", data$name, fixed = TRUE)) {
      return("Please submit a csv file.")
    }
    data <- readr::read_csv(data$datapath)
    x <- check_template(data, template)
    if(is.character(x)){
      return(x)
    }
    data
  })

  return(get_data)
}

## To be copied in the UI
# mod_data_upload_ui("data_upload_ui_1")

## To be copied in the server
# callModule(mod_data_upload_server, "data_upload_ui_1")

