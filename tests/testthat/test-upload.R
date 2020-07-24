test_that("data upload works", {
  data <- readr::read_csv(system.file("extdata/upload_example.csv", package = "shinyrems"))
  data_bad <- readr::read_csv(system.file("extdata/upload_example_bad.csv", package = "shinyrems"))

  expect_error(check_data_upload(data_bad))
  expect_equal(check_data_upload(data), data)

  data <- data %>%
    process_dates() %>%
    dplyr::select(-EMS_ID)

  # check that uploaded data passes through tabs
  tidy_data <- ems_tidy(data,
                        mdl_action = "zero",
                        dataset = "upload",
                        cols = character(0)
  )
  tidy_data2 <- wqbc::tidy_ems_data(data, mdl_action = "zero", cols = character(0))
  expect_identical(tidy_data, tidy_data2)

  data2 <- data %>%
    dplyr::select(Station, Variable, DateTime, Value, Units)

  stand_data <- ems_standardize(data, TRUE)
  stand_data2 <- ems_standardize(data2, TRUE)

  agg_data <- ems_aggregate(stand_data,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  agg_data2 <- ems_aggregate(stand_data2,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 30L)

  out_data <- ems_outlier(stand_data2, by = "Station", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(out_data), 30L)
  data

})
