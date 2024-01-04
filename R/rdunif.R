# Generate random sample from a discrete uniform distribution
#'
#' This function was in purrr before version 0.3.5 but deprecated in purrr 1.0.0 because it's not related to the core purpose of purrr.
#'
#' @export
#'


rdunif <- function (n, b, a = 1)
{
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
  a1 <- min(a, b)
  b1 <- max(a, b)
  sample(b1 - a1 + 1, n, replace = TRUE) + a1 - 1
}
