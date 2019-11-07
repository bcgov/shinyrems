clean_wqdata2 <- function (x, by = NULL, max_cv = Inf, sds = 10, ignore_undetected = TRUE,
                           large_only = TRUE, delete_outliers = FALSE, remove_blanks = FALSE,
                           messages = getOption("wqbc.messages", default = TRUE)){
  # assert_that(is.data.frame(x))
  # assert_that(is.null(by) || (is.character(by) && noNA(by)))
  # assert_that(is.number(max_cv))
  # assert_that(is.flag(messages) && noNA(messages))
  # check_scalar(sds, c(1, 100))
  # check_flag(ignore_undetected)
  # check_flag(large_only)
  # check_flag(delete_outliers)
  wqbc:::check_by(by, colnames(x), res_names = c("Value", "Outlier",
                                          "DetectionLimit"))
  x <- x[!is.na(x$Value), , drop = FALSE]
  if (remove_blanks) {
    if (!"SAMPLE_CLASS" %in% names(x)) {
      stop("SAMPLE_CLASS column must be present to remove blank records")
    }
    x <- x[!grepl("^[Bb]lank", x$SAMPLE_CLASS), ]
  }
  if (!tibble::has_name(x, "Date")) {
    if (tibble::has_name(x, "DateTime")) {
      if (messages)
        message("replacing DateTime column with Date")
      x$Date <- lubridate::date(x$DateTime)
      x$DateTime <- NULL
    }
    else x <- wqbc:::add_missing_columns(x, list(Date = as.Date("2000-01-01")),
                                  messages = messages)
  }
  wqbc:::check_class_columns(x, list(Date = "Date"))
  if ("DetectionLimit" %in% colnames(x))
    wqbc:::check_class_columns(x, list(DetectionLimit = "numeric"))
  if (messages)
    message("Cleaning water quality data...")
  res <- c("Date", "Variable", "Code", "Value", "Units", "DetectionLimit",
           "ResultLetter")
  wqbc:::check_by(by, colnames(x), res_names = res)
  x <- wqbc:::del_cols_not_in_y(x, c(res, by))
  if (is.null(by)) {
    x <- wqbc:::clean_wqdata_by(x, max_cv = max_cv, messages = messages)
  }
  else {
    x <- plyr::ddply(x, .variables = by, .fun = wqbc:::clean_wqdata_by,
                     max_cv = max_cv, messages = messages)
  }
  x %<>% wqbc:::identify_outliers(by = by, sds = sds, ignore_undetected = ignore_undetected,
                           large_only = large_only, messages = messages)
  if (delete_outliers) {
    x %<>% dplyr::filter_(~!is.na(Outlier) & !Outlier)
    if (messages)
      message("Deleted outliers.")
  }
  if (messages)
    message("Cleansed water quality data.")
  x
}


