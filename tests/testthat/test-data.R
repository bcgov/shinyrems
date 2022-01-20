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

test_that("ems data functions work", {
  data <- ems_data_which("2yr")
  emsid <- c("0121580", "0200036")
  param <- "pH"
  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")
  data <- ems_data("2yr",
    emsid = emsid, parameter = param,
    from_date = from, to_date = to, data
  )

  expect_identical(unique(data$EMS_ID), emsid)
  expect_identical(unique(data$PARAMETER), param)
  expect_true(as.Date(max(data$COLLECTION_START)) <= to)
  expect_identical(nrow(data), 49L)

  tidy_data <- ems_tidy(data,
    mdl_action = "zero",
    dataset = "2yr", cols = c("UPPER_DEPTH", "LOWER_DEPTH")
  )
  tidy_data2 <- wqbc::tidy_ems_data(data, mdl_action = "zero",
                                    cols = c("UPPER_DEPTH", "LOWER_DEPTH"))
  expect_identical(tidy_data, tidy_data2)

  stand_data <- ems_standardize(tidy_data, TRUE)
  stand_data2 <- wqbc::standardize_wqdata(tidy_data, TRUE)
  expect_identical(stand_data, stand_data2)

  agg_data <- ems_aggregate(stand_data,
    by = "Station", remove_blanks = TRUE,
    max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 30L)

  out_data <- ems_outlier(stand_data, by = c("Station", "UPPER_DEPTH"),
                          max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(out_data), 30L)

  limits <- wqbc::limits

  #### test plot
  data <- out_data
  data$Site_Renamed <- data$Station
  guideline <- data.frame(UpperLimit = rep(1, 2),
                          id = 1,
                          calculated = FALSE,
                          Guideline = rep("test", 2),
                          Variable = unique(data$Variable),
                          Date = c(rep(min(data$Date), 2),
                                   rep(max(data$Date), 2)))

  x <- ems_plot_data(data, date_range = c(from, to), timeframe = "Year")
  gp <- ems_plot_base(x, facet = "Station", scales = TRUE, ncol = 1) %>%
    ems_plot_add_geom(plot_type = "scatter", geom = c("show points"),
                      point_size = 1, line_size = 1, colour = "Station",
                      timeframe = "Year", palette = "Set1") %>%
    ems_plot_add_guideline(guideline = guideline, guideline_colour = "black")

  expect_is(gp, "ggplot")
  expect_named(gp)

  ### check demo data
  data <- ems_demo_data
  data <- data %>%
    dplyr::filter(EMS_ID %in% unique(data$EMS_ID)[1:2],
                  PARAMETER == unique(data$PARAMETER)[1])

  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")

  tidy_data <- ems_tidy(data,
                        mdl_action = "zero",
                        dataset = "2yr", cols = character(0)
  )
  tidy_data2 <- wqbc::tidy_ems_data(data, mdl_action = "zero", cols = character(0))
  expect_identical(tidy_data, tidy_data2)

})
