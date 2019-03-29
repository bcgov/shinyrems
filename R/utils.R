selectInputX <- function(..., label = "Select sites:", choices, selected = choices[1]) {
  selectizeInput(..., multiple = TRUE, label = label,
                 choices = choices,
                 selected = selected,
                 options = list(
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

max_db_date <- function(){
  Sys.Date()
}

min_db_date <- function(){
  as.Date("1964-01-01")
}

