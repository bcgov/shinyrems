test_that("data upload works", {
  data <- readr::read_csv(system.file("extdata/upload_data_cadmium.csv", package = "shinyrems"))
  data_bad <- readr::read_csv(system.file("extdata/upload_example_bad.csv", package = "shinyrems"))

  data$ResultLetter <- as.character(data$ResultLetter)
  expect_error(check_data_upload(data_bad))
  expect_equal(check_data_upload(data), data)

  data <- data %>%
    process_dates()
  all_data <- data

  data <- data %>%
    dplyr::filter(Variable == "CADMIUM DISSOLVED")

  # check that uploaded data passes through tabs
  tidy_data <- ems_tidy(data,
                        mdl_action = "zero",
                        dataset = "upload",
                        cols = character(0)
  )
  expect_identical(nrow(tidy_data), 611L)

  stand_data <- ems_standardize(data, TRUE)
  expect_identical(nrow(stand_data), 611L)

  agg_data <- ems_aggregate(stand_data,
                            by = "Station", remove_blanks = FALSE,
                            max_cv = Inf, FUN = max
  )
  expect_identical(nrow(agg_data), 611L)

  out_data <- ems_outlier(stand_data, by = "Station", max_cv = Inf, sds = 1, FUN = mean)
  expect_identical(nrow(stand_data), 611L)

  limits <- wqbc::limits

  #### test plot
  data <- out_data
  from <- as.Date("2018-01-02")
  to <- as.Date("2019-09-30")
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

})
