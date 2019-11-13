summarise_wqdata <- function(x){
  checkr::check_colnames(x, c("Site_Renamed", "Variable", "Units", "Value"))
  dt <- data.table::data.table(x)
  if(identical(x$EMS_ID, x$Site_Renamed)){
    data <- dt[, .(n = .N,
                   min = min(Value, na.rm = TRUE),
                   max = max(Value, na.rm = TRUE),
                   mean = mean(Value, na.rm = TRUE),
                   median = median(Value, na.rm = TRUE),
                   sd = sd(Value, na.rm = TRUE),
                   se = se(Value)),
               .(EMS_ID, Variable, Units)]
  } else {
    data <- dt[, .(n = .N,
                   min = min(Value, na.rm = TRUE),
                   max = max(Value, na.rm = TRUE),
                   mean = mean(Value, na.rm = TRUE),
                   median = median(Value, na.rm = TRUE),
                   sd = sd(Value, na.rm = TRUE),
                   se = se(Value)),
               .(Site_Renamed, Variable, Units)]
  }
  as.data.frame(data)
}

clean_wqdata2 <- function (x, by = NULL, max_cv = Inf, sds = 10, ignore_undetected = TRUE,
                           large_only = TRUE, delete_outliers = FALSE, remove_blanks = FALSE,
                           messages = getOption("wqbc.messages", default = TRUE), FUN = "mean"){
  # assert_that(is.data.frame(x))
  # assert_that(is.null(by) || (is.character(by) && noNA(by)))
  # assert_that(is.number(max_cv))
  # assert_that(is.flag(messages) && noNA(messages))
  # check_scalar(sds, c(1, 100))
  # check_flag(ignore_undetected)
  # check_flag(large_only)
  # check_flag(delete_outliers)
  # checkr::check_string(FUN, c("mean", "max", "median"))
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
    x <- clean_wqdata_by2(x, max_cv = max_cv, messages = messages, FUN = FUN)
  }
  else {
    x <- plyr::ddply(x, .variables = by, .fun = clean_wqdata_by2,
                     max_cv = max_cv, messages = messages, FUN = FUN)
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

clean_wqdata_by2 <- function (x, max_cv, messages, FUN) {
  if (anyDuplicated(x$Variable))
    x <- plyr::ddply(x, "Variable", clean_wqdata_variable2,
                     max_cv = max_cv, messages = messages, FUN = FUN)
  x
}

clean_wqdata_variable2 <- function (x, max_cv, messages, FUN){
  if (anyDuplicated(x$Date))
    x <- plyr::ddply(x, "Date", clean_wqdata_replicates2,
                     max_cv = max_cv, messages = messages, FUN = FUN)
  x
}

clean_wqdata_replicates2 <- function (x, max_cv, messages, FUN){
  n <- nrow(x)
  cv <- wqbc:::cv(x$Value)
  if (wqbc:::cv(x$Value) > max_cv && nrow(x) > 2) {
    x <- dplyr::arrange_(x, ~-Value)
    while (wqbc:::cv(x$Value) > max_cv && nrow(x) > 2) {
      x <- x[-which.max(wqbc:::abs_dev(x$Value)), ]
    }
  }
  if(FUN == "mean"){
    x$Value <- mean(x$Value)
  }
  if(FUN == "max"){
    x$Value <- max(x$Value)
  }
  if(FUN == "median"){
    x$Value <- median(x$Value)
  }
  if (!is.null(x$DetectionLimit))
    x$DetectionLimit <- mean(x$DetectionLimit)
  if (messages && n > nrow(x)) {
    message("Filtered ", n - nrow(x), " of ", n, " replicate values with a CV of ",
            signif(cv, 3), " for ", x$Variable[1], " on ", x$Date[1],
            ".")
  }
  x[1, , drop = FALSE]
}



