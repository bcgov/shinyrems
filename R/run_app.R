#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  message("checking for historic data...")
  # rems::download_historic_data(ask = FALSE)
  message("checking for most recent 2 years of data...")
  # rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
  shiny::runApp(system.file("app", package = "shinyrems"))
}
