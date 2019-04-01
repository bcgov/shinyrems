#' Run the Shiny Application
#'
#' @param run_mode A string indicating which dataset the app should use.
#' Possible choices are:
#' "demo" (loads a small demo dataset that does not require any downloading);
#' "2yr" (loads and prompts download/update of recent EMS data from 2018-01-01 onwards);
#' "historic" (loads and prompts download/update of historic EMS data from 1964-01-01 to 2018-01-02);
#' "all" (loads both recent and historic data).
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(run_mode = "demo") {
  checkr::check_vector(run_mode, c("demo", "2yr", "historic"))
  # if(run_mode != "demo") stop("Sorry the app only runs in demo mode at this time!")
  if(run_mode == "historic"){
    message("checking for historic data...")
    rems::download_historic_data(ask = FALSE)
    # message("checking for most recent 2 years of data...")
    # rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
  }

  if(run_mode == "2yr"){
    message("checking for most recent 2 years of data...")
    rems::get_ems_data("2yr", ask = FALSE, dont_get = TRUE)
  }

  shinyOptions(run_mode = run_mode)
  shiny::runApp(system.file("app", package = "shinyrems"))
}
