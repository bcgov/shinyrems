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
  expect_identical(nrow(tidy_data), 19L)

  stand_data <- ems_standardize(data, TRUE)
  expect_identical(nrow(stand_data), 19L)

  agg_data <- ems_aggregate(stand_data,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 19L)

  out_data <- ems_outlier(stand_data, by = "Station", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(stand_data), 19L)

  limits <- wqbc::limits

  #### test plot
  data <- out_data
  data$Site_Renamed <- data$Station
  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")

  x <- ems_plot_data(data, date_range = c(from, to), timeframe = "Year")
  gp <- ems_plot_base(x, facet = "Station") %>%
    ems_plot_add_geom(plot_type = "scatter", geom = c("show points"),
                      point_size = 1, line_size = 1, colour = "Station",
                      timeframe = "Year", palette = "Set1") %>%
    ems_plot_add_guideline(guideline = data.frame(UpperLimit = 7))

  expect_is(gp, "ggplot")
  expect_named(gp)

})
