#' @import shiny
app_server <- function(input, output,session) {

  observeEvent(input$browser,{
    browser()
  })

  callModule(mod_about_server, "about_ui_1")

  callModule(mod_ems_server, "ems_ui_1")

  callModule(mod_reference_server, "reference_ui_1")

}
