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
  expect_identical(nrow(tidy_data), 2L)

  stand_data <- ems_standardize(data, TRUE)
  expect_identical(nrow(stand_data), 2L)

  agg_data <- ems_aggregate(stand_data,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 2L)

  out_data <- ems_outlier(stand_data, by = "Station", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(stand_data), 2L)


})
