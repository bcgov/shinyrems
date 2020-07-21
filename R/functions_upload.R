check_data_upload <- function(data){
  if(!inherits(data, "data.frame")){
    abort_chk("must be data.frame")
  }
  return(data)
}

preprocess_data_upload <- function(data){
  return(data)
}

process_data_upload <- function(data, variable){
  # if(is.null(variable)) return(data)
  print(data)
  data <- dplyr::filter(data, Variable == variable)
  return(data)
}
