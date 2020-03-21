summarise_wqdata <- function(x){
  checkr::check_colnames(x, c("EMS_ID_Renamed", "Variable", "Units", "Value"))
  dt <- dplyr::as_tibble(x)
  if(identical(x$EMS_ID, x$EMS_ID_Renamed)){
    data <- dt %>%
      dplyr::group_by(EMS_ID, Variable, Units) %>%
      dplyr::summarise(n = dplyr::n(),
                       min = min(Value, na.rm = TRUE),
                       max = max(Value, na.rm = TRUE),
                       mean = mean(Value, na.rm = TRUE),
                       median = median(Value, na.rm = TRUE),
                       sd = sd(Value, na.rm = TRUE),
                       se = se(Value)) %>%
      dplyr::ungroup()
  } else {
    data <- dt %>%
      dplyr::group_by(EMS_ID_Renamed,
                      Variable,
                      Units) %>%
      dplyr::summarise(n = dplyr::n(),
                       min = min(Value, na.rm = TRUE),
                       max = max(Value, na.rm = TRUE),
                       mean = mean(Value, na.rm = TRUE),
                       median = median(Value, na.rm = TRUE),
                       sd = sd(Value, na.rm = TRUE),
                       se = se(Value)) %>%
      dplyr::ungroup()
  }
  dplyr::as_tibble(data)
}




