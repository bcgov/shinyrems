summarise_wqdata <- function(x){
  checkr::check_colnames(x, c("EMS_ID_Renamed", "Variable", "Units", "Value"))
  dt <- dplyr::as_tibble(x)
  if(identical(x$EMS_ID, x$EMS_ID_Renamed)){
    data <- dt %>%
      dplyr::group_by(.data$EMS_ID, .data$Variable, .data$Units) %>%
      dplyr::summarise(n = dplyr::n(),
                       min = min(.data$Value, na.rm = TRUE),
                       max = max(.data$Value, na.rm = TRUE),
                       mean = mean(.data$Value, na.rm = TRUE),
                       median = median(.data$Value, na.rm = TRUE),
                       sd = sd(.data$Value, na.rm = TRUE),
                       se = se(.data$Value)) %>%
      dplyr::ungroup()
  } else {
    data <- dt %>%
      dplyr::group_by(.data$EMS_ID_Renamed,
                      .data$Variable,
                      .data$Units) %>%
      dplyr::summarise(n = dplyr::n(),
                       min = min(.data$Value, na.rm = TRUE),
                       max = max(.data$Value, na.rm = TRUE),
                       mean = mean(.data$Value, na.rm = TRUE),
                       median = median(.data$Value, na.rm = TRUE),
                       sd = sd(.data$Value, na.rm = TRUE),
                       se = se(.data$Value)) %>%
      dplyr::ungroup()
  }
  dplyr::as_tibble(data)
}
