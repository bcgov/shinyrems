# create_site_parameter_lookup <- function(){
#   db_path <- rems:::write_db_path()
#   con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
#   on.exit(DBI::dbDisconnect(con))
#   con %>%
#     dplyr::tbl("historic") %>%
#     dplyr::group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE, PARAMETER) %>%
#     dplyr::summarise() %>%
#     dplyr::ungroup() %>%
#     dplyr::collect()
# }
#
# create_site_lookup <- function(){
#   data <- rems:::._remsCache_$exists()
#   data %>%
#     dplyr::group_by(EMS_ID, MONITORING_LOCATION, LATITUDE, LONGITUDE, LOCATION_TYPE) %>%
#     dplyr::summarise() %>%
#     dplyr::ungroup()
# }
#
# # need to add site_parameter_lookup to cache
# # chan check if exists with !._remsCache_$exists("site_parameter")
#
# # if 2yr data has been update then update lookup table
# # check dates available with ._remsCache_$exists("cache_dates")
# # get cache date with get_cache_date("2yr")
# # get file metadata with get_file_metadata("2yr")
# # if (cache_date < file_meta[["server_date"]]) then update lookup tables
#
# cache_lookup <- function(){
#
# }
#
# check_cache_lookup <- function(){
#
# }
