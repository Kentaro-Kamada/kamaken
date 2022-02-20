# Discriptive statistics for categorical variables
#'
#' Show Discriptive statistics for categorical variables
#'
#' @param .data input a tibble or data.frame
#' @param ... categorical variables
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

describe_d <- function(.data, ...){
  .data <- dplyr::select(.data, ...)
  purrr::map2_dfr(.x = .data, .y = names(.data),
                  .f =  ~{
                    tab <- janitor::tabyl(.x)
                    variable_name <- .y
                    N <- tab[,2] %>% sum()
                    dplyr::tibble(
                      variables = c(variable_name, stringr::str_c('  ', tab[,1])),
                      N = c(N, tab[,2]),
                      percent = c(1, tab[,3]))
                  })
}
