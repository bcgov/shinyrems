# Module UI

#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(1,
           HTML("")),
    column(11,
           h4(paste("Welcome!")),
           br(),
           h5("This app is an interface to the rems R package. For more information about rems, see the ",
              a("GitHub page.", href = "https://github.com/poissonconsulting/rems")),
           actionLink(ns('info_citation'), "Citation info"),
           shinyjs::hidden(div(id = ns("div_citation"),
                      h6(HTML(paste("To cite package 'rems' in publications, use:<br><br>

                                    Joe Thorley (2018). ypr: Yield Per Recruit. R package version",
                                    packageVersion('ypr'),
                                    "https://github.com/poissonconsulting/ypr <br><br>

                                    To cite app: <br><br>
                                    Seb Dalgarno (2018). shinyrems: An app for download and visualization of ems data.
                                    https://poissonconsulting.shinyapps.io/ypr/<br><br>
                                    "
                      ))))),
           hr(),
           h6('Developed by Poisson Consulting.')),
    tags$footer(actionLink(inputId = 'poisson',
                           label = img(src = 'https://www.poissonconsulting.ca/assets/logos/poisson.png',
                                       height = 177/5,
                                       width = 739/5,
                                       onclick = "window.open('http://www.poissonconsulting.ca', '_blank')")),
                align = "center",
                style = "position: relative;
                bottom:1;
                width:100%;
                height:50px; /* Height of the footer */
                color: #2f4f4f;
                padding: 10px;
                background-color: white;
                z-index: -1000;
                font-size: 12px"))
}

# Module Server

#' @rdname mod_about
#' @export
#' @keywords internal

mod_about_server <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$info_citation, {
    shinyjs::toggle("div_citation", anim = TRUE, animType = "slide", time = 0.2)
  })
}


