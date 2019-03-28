#' @import shiny
app_ui <- function() {
  tagList(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    golem::js(),
    golem::favicon(),
    navbarPage(title =  "EMS Database", selected = 'EMS',
               tabPanel(title = 'EMS',
                        br(),
                        mod_ems_ui("ems_ui_1")),
               tabPanel(title = 'Site Reference',
                        br(),
                        mod_site_ui("site_ui_1")),
               tabPanel(title = 'About',
                        br(),
                        mod_about_ui("about_ui_1")
                        ))
  )
}

golem_add_external_resources <- function(){
  addResourcePath( 'www', system.file('app/www', package = 'shinyrems'))
  tagList(tags$link(rel="stylesheet", type="text/css", href="www/style.css"))
}
