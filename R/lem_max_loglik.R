#
#'
#'
#' @export
#'


lem_max_loglik <- function(out_path) {

  # 最大対数尤度の読み出し
  tibble(
    filepath = list.files(out_path, full.names = T),
    loglik = map_chr(filepath, read_file)
  ) %>%
    mutate(
      seed = str_extract(loglik, '(?<=Seed random values   = ).+') %>%
        parse_double(),
      loglik = str_extract(loglik, '(?<=Log-likelihood       = ).+') %>%
        parse_double()
    ) %>%
    arrange(desc(loglik)) %>%
    mutate(rank = dense_rank(desc(loglik)))
}
