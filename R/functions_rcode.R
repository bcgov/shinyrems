pbr <- function(x){
  HTML(paste(x, collapse = "<br/>"))
}

pc <- function(x){
  paste0("c('", paste(x, collapse = "', '"), "')")
}

pcnum <- function(x){
  paste0("c(", paste(x, collapse = ", "), ")")
}

rcode_head <- function(dataset){
  l <- list()

  if(dataset == "upload"){
    l$readr <- "library(readr)"
  }

  l$rems <- "library(rems)"
  l$wqbc <- "library(wqbc)"
  l$ggplot2 <- "library(ggplot2)"
  l$dplyr <- "library(dplyr)"

  pbr(l)
}

rcode_data <- function(dataset, emsid, parameter, date, file){

  l <- list()

  l$comment <- "### get data"
  l$emsid <- paste0("emsid <- ", pc(emsid))
  l$parameter <- paste0("parameter <- ", pc(parameter))
  l$from <- paste0("from_date <- '", date[1], "'")
  l$to <- paste0("to_date <- '", date[2], "'")

  l$data <- "data <- rems::ems_demo_data"
  l$filter <- paste0("data <- rems::filter_ems_data(x = data, emsid = emsid,
             parameter = parameter, from_date = from_date, to_date = to_date)")

  if(dataset == "2yr"){
    l$data <- "data <- rems::get_ems_data(which = '2yr', dont_update = TRUE, force = TRUE)"
  }

  if(dataset == "4yr"){
    l$data <- "data <- rems::get_ems_data(which = '4yr', dont_update = TRUE, force = TRUE)"
  }

  if(dataset == "historic"){
    l$data <- character(0)
    l$filter <- "data <- rems::read_historic_data(emsid = emsid,
                                               parameter = parameter,
                                               from_date = from_date,
                                               to_date = to_date,
                                               check_db = FALSE)"

  }

  if(dataset == "all"){
    l$data <- "data <- rems::get_ems_data(which = '2yr', dont_update = TRUE, force = TRUE)"
    l$filter <- "data <- rems::bind_ems_data(
           rems::read_historic_data(emsid = emsid,
                                    parameter = parameter,
                                    from_date = from_date,
                                    to_date = to_date,
                                    check_db = FALSE),
           rems::filter_ems_data(x = data,
                                 emsid = emsid,
                                 parameter = parameter,
                                 from_date = from_date,
                                 to_date = to_date)
         )"
  }

  if(dataset == "upload"){
    l <- list()
    l$comment <- "# you will need to change the 'file' argument to point to the file path"
    l$data <- glue("data <- readr::read_csv('{file}')")
  }
  pbr(l)
}

rcode_tidy <- function(mdl_action, cols){
  l <- list()
  l$comment <- "### tidy data"
  l$cols <- paste0("cols <- ", pc(cols))
  l$data <- glue("data <- wqbc::tidy_ems_data(data, mdl_action = '{mdl_action}')")
  if(length(cols) > 0){
    l$data <- glue("data <- wqbc::tidy_ems_data(data, mdl_action = {mdl_action},
                 cols = cols)")
  }
  pbr(l)
}

rcode_standardize <- function(strict){
  l <- list()
  l$comment <- "### standardize data"
  l$data <- glue("data <- wqbc::standardize_wqdata(data, strict = {strict})")
  pbr(l)
}

rcode_clean <- function(by, max_cv, remove_blanks, fun){

  l <- list()
  l$comment <- "### clean data"
  l$by <- paste0("by <- ", pc(by))
  l$data <- glue("data <- wqbc::clean_wqdata(data, by = by, max_cv = {max_cv},
                  remove_blanks = {remove_blanks}, FUN = {fun})")
  pbr(l)
}

rcode_clean2 <- function(by, max_cv, sds,
                        ignore_undetected, large_only,
                        remove_blanks, fun){

  l <- list()
  l$comment <- "### clean data"
  l$by <- paste0("by <- ", pc(by))
  l$data <- glue("data <- wqbc::clean_wqdata(data, by = by, max_cv = {max_cv},
                 sds = {sds}, ignore_undetected = {ignore_undetected},
                 large_only = {large_only}, remove_blanks = {remove_blanks},
                 FUN = {fun})")
  pbr(l)
}

rcode_outlier <- function(manual_outliers){
  l <- list()
  l$comment <- "### remove outliers"
  if(length(manual_outliers) > 0){
    l$outliers <- paste0("outlier_index <- ", pcnum(manual_outliers))
    l$manual <- "data$Outlier[outlier_index] <- TRUE"
  }
  l$data <- "data <- data[!data$Outlier,]"
  pbr(l)
}

rcode_result_plot <- function(){
  l <- list()


}

rcode_result_summary <- function(){
  l <- list()

}
