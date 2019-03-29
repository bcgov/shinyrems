#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  message("Before loading the app, we'll check that you have the required datasets on your computer: 2yr and historic...If there are newer verions available you may choose to update them.")
  check_historic_data()
  # check_2yr_data()
  # check_bound_data()
  shiny::runApp(system.file("app", package = "shinyrems"))
}
