# Create a list of value labels from SPSS or Stata data
#'
#' Create a list of value labels from SPSS or Stata data
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom purrr attr_getter
#' @importFrom tidyr unnest
#' @importFrom haven is.labelled
#'
#' @export
#'



haven_value_label <- function(data) {
  if(!(class(data) %in% c('data.frame', 'tbl_df'))) stop('`data` must be `data.frame` or `tibble`')
  else {
    tibble(
      変数 = names(select(data, where(is.labelled))),
      値 = map(select(data, where(is.labelled)),
              ~pluck(., attr_getter('labels'))),
      ラベル = map(値, names)
    ) %>%
      unnest(cols = c(値, ラベル))
  }
}
