# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


db_path <- rems:::write_db_path()
csv_file <- "~/Poisson/Data/shinyrems/ems_sample_results_current_expanded.csv"
n <- 1e+06
# file_meta <- rems:::get_file_metadata("historic")
rems:::save_historic_data(csv_file, db_path, n)
rems:::set_cache_date("historic", Sys.Date())
# rems:::set_cache_date("historic", file_meta[["server_date"]])
