# Wrapper function of dplyr::na_if
#'
#' This is a wrapper function of `dplyr::na_if`. This function allows us input a numeric vector to `y`, while `dplyr::na_if` only recieves a scalar.
#'
#' @param x a variable vector
#' @param y a numeric vector which contains values you want to convert to `NA`
#'
#' @export
#'

na.if <- function(x, y){
  x[x %in% y] <- NA
  return(x)
}
