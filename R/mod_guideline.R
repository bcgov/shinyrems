# Module UI

#' @title   mod_guideline_ui and mod_guideline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_guideline
#'
#' @keywords internal
#' @export
mod_guideline_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(class = "sidebar",
                   ),
      mainPanel(tabsetPanel(selected = "Plot",
                            tabPanel(title = "Plot",
                                     br(),
                                     dl_group("plot", ns),
                                     br2(),
                                     uiOutput(ns("ui_plot"))),
                            tabPanel(title = "R Code",
                                     br(),
                                     wellPanel(uiOutput(ns("rcode"))))
      ))
    )
  )
}

# Module Server

#' @rdname mod_guideline
#' @export
#' @keywords internal

mod_guideline_server <- function(input, output, session, results){
  ns <- session$ns
}



