is_try_error <- function(x) inherits(x, "try-error")

se <- function(x){
  sd(x, na.rm=TRUE) /
    sqrt(length(x[!is.na(x)]))
}
