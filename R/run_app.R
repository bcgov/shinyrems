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
run_app <- function(run_mode = "2yr") {
  checkr::check_vector(run_mode, c("demo", "2yr", "historic", "all"))

  switch(run_mode,
         "2yr" = check_2yr_data(),
         "historic" = check_historic_data(),
         "all" = check_all_data())

  shinyOptions(run_mode = run_mode)
  shiny::runApp(system.file("app", package = "shinyrems"))
}
