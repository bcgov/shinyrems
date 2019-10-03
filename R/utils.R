is_try_error <- function(x) inherits(x, "try-error")

check_template <- function(x, template){
  x <- try(checkr::check_data(x = x,
                     values = sapply(template, function(x) x$check, USE.NAMES = FALSE),
                     nrow = c(1L,.Machine$integer.max),
                     error = TRUE, x_name = "data"), silent = TRUE)
  if(is_try_error(x))
    return(gsub("Error : |\n", "", x[1]))
  invisible(TRUE)
}

template_to_df <- function(template){
  x <- tibble::tibble(variable = c("example", "type", "description"))
  for(i in names(template)){
    x[x$variable == "example", i] <- template[[i]][["example"]]
    x[x$variable == "type", i] <- template[[i]][["type"]]
    x[x$variable == "description", i] <- template[[i]][["description"]]
  }
  x[,-1]
}
