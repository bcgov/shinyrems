#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  shinyrems:::check_historic_db()
  shiny::runApp(system.file("app", package = "shinyrems"))
}
