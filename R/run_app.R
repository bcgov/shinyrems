#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(run_mode = "demo") {
  if(run_mode != "demo") stop("Sorry the app only runs in demo mode at this time!")
  if(run_mode == "all"){
    message("checking for historic data...")
    rems::download_historic_data(ask = FALSE)
    message("checking for most recent 2 years of data...")
    rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
  }

  if(run_mode == "2yr"){
    message("checking for most recent 2 years of data...")
    rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
  }

  shinyOptions(run_mode = run_mode)
  # source(system.file("app", package = "shinyrems", local = TRUE, chdir = TRUE))$value

  shiny::runApp(system.file("app", package = "shinyrems"))
}
