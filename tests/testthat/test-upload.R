test_that("data upload works", {
  data <- readr::read_csv(system.file("extdata/upload_example.csv", package = "shinyrems"))
  data_bad <- readr::read_csv(system.file("extdata/upload_example_bad.csv", package = "shinyrems"))

  expect_error(check_data_upload(data_bad))
  expect_equal(check_data_upload(data), data)

  data <- data %>%
    process_dates()

  # check that uploaded data passes through tabs
  tidy_data <- ems_tidy(data,
                        mdl_action = "zero",
                        dataset = "upload",
                        cols = character(0)
  )
  expect_identical(nrow(tidy_data), 3L)

  stand_data <- ems_standardize(data, TRUE)
  expect_identical(nrow(stand_data), 2L)

  agg_data <- ems_aggregate(stand_data,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 2L)

  out_data <- ems_outlier(stand_data, by = "EMS_ID", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(stand_data), 2L)

  limits <- wqbc::limits

  #### test plot
  data <- out_data
  data$EMS_ID_Renamed <- data$EMS_ID
  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")

  x <- ems_plot(data,
                plot_type = "scatter", geom = c("show lines", "show points"),
                date_range = c(from, to), point_size = 1, line_size = 1,
                facet = "EMS_ID", colour = "EMS_ID", timeframe = "Year", guideline = 6
  )
  expect_is(x, "ggplot")
  expect_named(x)

})
