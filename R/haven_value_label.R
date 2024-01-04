#' Create a list of value labels from SPSS or Stata data
#'
#' Create a list of value labels from SPSS or Stata data
#'
#' @importFrom dplyr tibble select
#' @importFrom purrr map pluck attr_getter
#' @importFrom rlang inherits_any
#' @importFrom tidyr unnest
#' @importFrom haven is.labelled
#'
#' @export
#'



haven_value_label <- function(data) {
  if(!inherits_any(data, c('data.frame', 'tbl_df'))) stop('`data` must be `data.frame` or `tibble`')
  else {
    tibble(
      変数 = names(select(data, where(is.labelled))),
      値 = map(select(data, where(is.labelled)),
              \(x) pluck(x, attr_getter('labels'))),
      ラベル = map(値, names)
    ) %>%
      mutate(値 = map(値, as.character)) %>%
      unnest(cols = c(値, ラベル))
  }
}
