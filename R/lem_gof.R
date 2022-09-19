#
#'
#'
#'
#' @export
#'


lem_gof <- function(path) {
  output <- read_file(path)
  tibble(
    logLik = str_extract(output, '(?<=Log-likelihood       = ).+') %>% parse_double(),
    AIC = str_extract(output, '(?<=AIC\\(log-likelihood\\)  = ).+') %>% parse_double(),
    BIC = str_extract(output, '(?<=BIC\\(log-likelihood\\)  = ).+') %>% parse_double(),
    g.squared = str_extract(output, '(?<=L-squared            = ).+(?=\\(.+\\))') %>%
      parse_double(),
    chi.squared = str_extract(output, '(?<=X-squared            = ).+(?=\\(.+\\))') %>%
      parse_double(),
    df.residual = str_extract(output, '(?<=Degrees of freedom   = ).+') %>%
      parse_double()
  )
}

