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


test_that("calculating guideline works", {

  # site <- c("FRASER RIVER AT RED PASS.", "FRASER RIVER AT MARGUERITE")
  emsid <- c("0400764")
  param <- c("Zinc Total")
  from_date <- as.Date("1990-01-02")
  to_date <- as.Date("2019-09-30")
  mdl_action <- "zero"
  data_type <- "raw"
  dataset <- "historic"
  cols <- character(0)
  strict <- TRUE
  by <- "EMS_ID"
  max_cv <- Inf
  sds <- 1
  FUN <- mean
  ignore_undetected <- TRUE
  large_only <- TRUE
  remove_blanks <- TRUE
  lookup <- lookup_historic

  data <- ems_data(dataset,
    emsid = emsid, parameter = param,
    from_date = from_date, to_date = to_date, data
  ) %>%
    ems_tidy(
      mdl_action = mdl_action, data_type = data_type,
      dataset = dataset, cols = cols
    ) %>%
    ems_standardize(strict) %>%
    ems_outlier(
      by = by, max_cv = max_cv, sds = sds,
      ignore_undetected = ignore_undetected, large_only = large_only,
      remove_blanks = remove_blanks, FUN = FUN
    )

  ### get data for additional params - Hardness
  # this doesnt work because missing data for hardness
  x <- wqbc::calc_limits(data, by = "EMS_ID", term = "long", estimate_variables = FALSE)
  expect_identical(nrow(x), 0L)
  expect_identical(code_to_parameter("EMS_0107", get_lookup(dataset)), "Hardness Total (Total)")

  # get additional param data
  params <- additional_parameters(data, lookup)
  expect_identical(params, "Hardness Total (Total)")

  data2 <- ems_data_parameter(data,
    all_data = NULL, dataset = "historic",
    lookup = lookup,
    from_date = from_date, to_date = to_date,
    mdl_action = mdl_action, cols = cols, strict = strict,
    by = by, sds = sds, ignore_undetected = ignore_undetected,
    large_only = large_only, remove_blanks = remove_blanks,
    max_cv = max_cv, FUN = FUN, limits = wqbc::limits
  )

  all_data <- rbind(data, data2)
  x <- wqbc::calc_limits(all_data, clean = FALSE, term = "long", estimate_variables = TRUE)
  expect_identical(nrow(x), 1L)

  y <- wqbc::calc_limits(all_data, clean = FALSE, term = "long", estimate_variables = FALSE)
  expect_false(identical(x, y))

  x <- wqbc::calc_limits(all_data, by = "EMS_ID", clean = FALSE, term = "short", estimate_variables = TRUE)
  expect_identical(nrow(x), 113L)

  y <- wqbc::calc_limits(all_data, clean = FALSE, term = "long-daily", estimate_variables = FALSE)
  expect_identical(nrow(y), 109L)
  expect_false(identical(x, y))

  z <- try(wqbc::calc_limits(data2, clean = FALSE, term = "long-daily", estimate_variables = FALSE),
    silent = TRUE
  )

  data$EMS_ID_Renamed <- data$EMS_ID

  #####
  date_range <- c(from_date, to_date)
  timeframe <- "Year"
  facet <- "EMS_ID"
  guideline <- x
  data$Detected <- detected(data$Value, data$DetectionLimit)
  data$EMS_ID <- data$EMS_ID_Renamed
  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data <- data[data$Date >= as.Date(date_range[1]) & data$Date <= as.Date(date_range[2]), ]
  data$Timeframe <- factor(get_timeframe(data$Date, timeframe))

  gp <- ems_plot(
    data, "scatter", c("show lines", "show points"),
    c(from_date, to_date), 1, 0.5, "Variable", "EMS_ID", "Year", y
  )

  expect_is(gp, "ggplot")

  emsid <- c("E103448")
  param <- c("Aluminum Dissolved")

  data <- ems_data(dataset,
    emsid = emsid, parameter = param,
    from_date = from_date, to_date = to_date, data
  ) %>%
    ems_tidy(
      mdl_action = mdl_action, data_type = data_type,
      dataset = dataset, cols = cols
    ) %>%
    ems_standardize(strict) %>%
    ems_outlier(
      by = by, max_cv = max_cv, sds = sds,
      ignore_undetected = ignore_undetected, large_only = large_only,
      remove_blanks = remove_blanks, FUN = FUN
    )

  ### get data for additional params - Hardness
  # this doesnt work because missing data for hardness
  x <- wqbc::calc_limits(data, by = "EMS_ID", term = "long", estimate_variables = FALSE)
  expect_identical(nrow(x), 0L)

  # get additional param data
  params <- additional_parameters(data, lookup)
  expect_identical(params, "pH")

  data2 <- ems_data_parameter(data,
    all_data = NULL, dataset = "historic",
    lookup = lookup,
    from_date = from_date, to_date = to_date,
    mdl_action = mdl_action, cols = cols, strict = strict,
    by = by, sds = sds, ignore_undetected = ignore_undetected,
    large_only = large_only, remove_blanks = remove_blanks,
    max_cv = max_cv, FUN = FUN, limits = wqbc::limits
  )

  all_data <- rbind(data, data2)
  x <- wqbc::calc_limits(all_data, clean = FALSE, term = "long", estimate_variables = TRUE)
  expect_identical(nrow(x), 242L)

  y <- wqbc::calc_limits(all_data, clean = FALSE, term = "short", estimate_variables = TRUE)
  expect_identical(nrow(y), 5933L)
})
