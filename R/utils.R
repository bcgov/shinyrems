selectInputX <- function(...) {
  selectizeInput(..., multiple = TRUE, options = list(
    'plugins' = list('remove_button'),
    'create' = TRUE,
    'persist' = FALSE))
}

emsTableOutput <- function(...){
  wellPanel(dataTableOutput(...), class = "wellpanel")
}

emsDownload <- function(..., label = "Download Data (csv)", br = TRUE){
  if(br){
    return(tagList(
      br(),
      downloadButton(..., class = 'small-dl',
                     label = label),
      br(), br()
    ))
  }
  downloadButton(..., class = 'small-dl',  label = label)
}

leaflet_labels <- function(data){
  name <- names(data)
  lapply(1:nrow(data), function(x){
    data <- data[x,]
    paste0("<strong>", name, ": </strong>", data[name], "<br>")
  })
}

check_historic_data <- function(){
  rems::download_historic_data(ask = FALSE)
}

check_2yr_data <- function(){
 rems::get_ems_data("2yr", ask = FALSE)
}

check_bound_data <- function(){

}

max_db_date <- function(){
  Sys.Date()
}

min_db_date <- function(){
  as.Date("1964-01-01")
}

