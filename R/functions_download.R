download_data <- function(which, shiny_session, shiny_id){
  switch(which,
         "2yr" = get_ems_data2(which = "2yr", force = TRUE, ask = FALSE,
                               shiny_progress = TRUE, shiny_session = shiny_session,
                               shiny_id = shiny_id),
         "historic" = rems::download_historic_data(force = TRUE, ask = FALSE))
}

