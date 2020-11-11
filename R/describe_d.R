describe_d <- function(.data, ...){
  .data <- select(.data, ...)
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
