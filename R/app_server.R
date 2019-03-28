#' @import shiny
app_server <- function(input, output,session) {

  observeEvent(input$browser,{
    browser()
  })

  callModule(mod_about_server, "about_ui_1")

  callModule(mod_ems_server, "ems_ui_1")

  callModule(mod_site_server, "site_ui_1")

}
