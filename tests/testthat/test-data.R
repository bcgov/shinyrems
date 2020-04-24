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

context("test-data")

test_that("ems data functions work", {

  data <- ems_data_which("2yr")
  emsid <- c("0121580", "0200036")
  param <- "pH"
  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")
  data <- ems_data("2yr", emsid = emsid, parameter = param,
                   from_date = from, to_date = to, data)

  expect_identical(unique(data$EMS_ID), emsid)
  expect_identical(unique(data$PARAMETER), param)
  expect_true(as.Date(max(data$COLLECTION_START)) <= to)
  expect_identical(nrow(data), 48L)

  tidy_data <- ems_tidy(data, mdl_action = "zero", data_type = "raw",
                        dataset = "2yr", cols = character(0))
  tidy_data2 <- wqbc::tidy_ems_data(data, mdl_action = "zero", cols = character(0))
  expect_identical(tidy_data, tidy_data2)

  stand_data <- ems_standardize(tidy_data, TRUE)
  stand_data2 <- wqbc::standardize_wqdata(tidy_data, TRUE)
  expect_identical(stand_data, stand_data2)

  agg_data <- ems_aggregate(stand_data, by = "EMS_ID", remove_blanks = TRUE,
                            max_cv = Inf, FUN = max)
  expect_identical(nrow(agg_data), 30L)

  out_data <- ems_outlier(stand_data, by = "EMS_ID", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(out_data), 30L)

  limits <- wqbc::limits

  #### test plot
  data = out_data
  data$EMS_ID_Renamed <- data$EMS_ID

  x <- ems_plot(data, plot_type = "scatter", geom = c("show lines" ,"show points"),
            date_range = c(from, to), point_size = 1, line_size = 1,
            facet = "EMS_ID", colour = "EMS_ID", timeframe = "Year", guideline = 6)

  expect_is(x, "ggplot")
  expect_named(x)

})
