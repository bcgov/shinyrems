p_br <- function(x){
  HTML(paste(x, collapse = "<br/>"))
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

  p_br(l)
}

rcode_data <- function(dataset, emsid, parameter, date,
                       sample_state, sample_class, mdl_action, file){

  l <- list()

  l$comment <- "### get data"
  l$emsid <- paste0("emsid <- c('", paste(emsid, collapse = "', '"), "')")
  l$parameter <- paste0("parameter <- c('", paste(parameter, collapse = "', '"), "')")
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

  p_br(l)
}

rcode_standardize <- function(data, strict){
  l <- list()
  l$comment <- "### standardize data"
  l$data <- glue("data <- wqbc::standardize_wqdata(data, strict = {strict})")
  p_br(l)
}
