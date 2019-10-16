# Module UI

#' @title   mod_data_check_ui and mod_data_check_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_check
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_data_check_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("check_data"))
  )
}

# Module Server

#' @rdname mod_data_check
#' @export
#' @keywords internal

mod_data_check_server <- function(input, output, session, dataset, check, which){
  ns <- session$ns

  observe({
    check <- check()
    dataset <- dataset()
    which <- which()

    if(check == "download"){
      output$check_data <- renderUI({
        tagList(
          p(glue("You don't have the {which} dataset.
                 Would you like to download it?")),
          button(ns("yes_download"), "Yes!", icon = icon(NULL)),
          button(ns("no_download"), "No thanks", icon = icon(NULL)),
          br(),
          textOutput(ns("download_text"))
        )
      })
    }
    if(check == "update"){
      output$check_data <- renderUI({
        tagList(
          p(glue("There is a newer version of the {which} dataset available.
                 Would you like to download it?")),
          button(ns("yes_download"), "Yes!", icon = icon(NULL)),
          button(ns("no_update"), "No thanks", icon = icon(NULL)),
          br(),
          textOutput(ns("download_text"))
        )
      })
    }
  })
}

## To be copied in the UI
# mod_data_check_ui("data_check_ui_1")

## To be copied in the server
# callModule(mod_data_check_server, "data_check_ui_1")

