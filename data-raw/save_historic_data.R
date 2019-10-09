db_path <- rems:::write_db_path()
csv_file <- "~/Poisson/Data/shinyrems/ems_sample_results_current_expanded.csv"
n <- 1e+06
file_meta <- rems:::get_file_metadata("historic")
rems:::save_historic_data(csv_file, db_path, n)
rems:::set_cache_date("historic", file_meta[["server_date"]])
