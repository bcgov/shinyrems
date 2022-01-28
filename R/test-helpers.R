save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_data <- function(x, name) {
  testthat::skip_on_ci()
  testthat::skip_on_os("windows")
  path <- save_csv(x)
  testthat::expect_snapshot_file(path, paste0(name, ".csv"))
}


save_png <- function(x) {
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(path, plot = x)
  path
}

expect_snapshot_plot <- function(x, name) {
  testthat::skip_on_ci()
  testthat::skip_on_os("windows")
  path <- save_png(x)
  testthat::expect_snapshot_file(path, paste0(name, ".png"))
}
