is_try_error <- function(x) inherits(x, "try-error")

template_to_df <- function(template){
  x <- tibble::tibble(variable = c("example", "type", "description"))
  for(i in names(template)){
    x[x$variable == "example", i] <- template[[i]][["example"]]
    x[x$variable == "type", i] <- template[[i]][["type"]]
    x[x$variable == "description", i] <- template[[i]][["description"]]
  }
  x[,-1]
}

template_to_empty <- function(template){
  setNames(data.frame(matrix(ncol = length(names(template)), nrow = 0)), names(template))
}

se <- function(x){
  sd(x, na.rm=TRUE) /
    sqrt(length(x[!is.na(x)]))
}
