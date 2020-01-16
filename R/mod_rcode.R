# Module UI

#' @title   mod_rcode_ui and mod_rcode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_rcode
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_rcode_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput("ui_help"),
    div(id = 'codes',
        wellPanel(
          uiOutput(ns('code_head')),
          br(),
          uiOutput(ns("code_data")),
          br(),
          uiOutput(ns("code_tidy")),
          br(),
          uiOutput(ns("code_standardize")),
          br(),
          uiOutput(ns("code_clean"))
    )
  ))
}

# Module Server

#' @rdname mod_rcode
#' @export
#' @keywords internal

mod_rcode_server <- function(input, output, session, tidy,
                             stand, clean, outlier, results){
  ns <- session$ns

  dataset <- getShinyOption("dataset", "demo")

  output$code_head <- renderUI({
    rcode_head(dataset)
  })

  output$code_data <- renderUI({
    rcode_data(dataset, emsid = tidy$emsid(),
               parameter = tidy$parameter(),
               date = tidy$date(),
               sample_state = tidy$sample_state(),
               sample_class = tidy$sample_class(),
               mdl_action = tidy$mdl_action(),
               file = tidy$file())
  })

  output$code_tidy <- renderUI({
    if(dataset == "upload" && tidy$data_type() == "tidy"){
      return()
    }
    rcode_tidy(mdl_action = tidy$mdl_action(), cols = tidy$cols())
  })

  output$code_standardize <- renderUI({
    rcode_standardize(strict = stand$strict())
  })

  output$code_clean <- renderUI({
    rcode_clean(by = outlier$by(), max_cv = outlier$max_cv(),
                sds = outlier$sds(), ignore_undetected = outlier$ignore_undetected(),
                large_only = outlier$large_only(), remove_blanks = outlier$remove_blanks(),
                fun = outlier$fun())
  })
}

## To be copied in the UI
# mod_rcode_ui("rcode_ui_1")

## To be copied in the server
# callModule(mod_rcode_server, "rcode_ui_1")

