selectInputX <- function(...) {
  selectizeInput(..., multiple = TRUE, options = list(
    'plugins' = list('remove_button'),
    'create' = TRUE,
    'persist' = FALSE))
}

check_historic_db <- function(){
  db_path <- rems:::write_db_path()
  if (!file.exists(db_path)){
    stop("You have not downloaded the historic database. Please follow instructions at 'https://github.com/poissonconsulting/shinyrems'")
  }
}

max_db_date <- function(){
  db_path <- rems:::write_db_path()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT MAX(COLLECTION_START) FROM historic") %>%
    rems::ems_posix_numeric() %>%
    as.Date()
}

min_db_date <- function(){
  db_path <- rems:::write_db_path()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT MIN(COLLECTION_START) FROM historic") %>%
    rems::ems_posix_numeric() %>%
    as.Date()
}

