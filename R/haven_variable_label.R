#' Create a list of variable labels from SPSS or Stata data
#'
#' Create a list of variable labels from SPSS or Stata data
#'
#' @importFrom dplyr tibble
#' @importFrom purrr map_chr pluck attr_getter is_null
#' @importFrom rlang inherits_any
#'
#' @export
#'

haven_variable_label <- function(data) {
  if(!inherits_any(data, c('data.frame', 'tbl_df'))) stop('`data` must be `data.frame` or `tibble`')
  else {
    tibble(
      変数 = names(data),
      位置 = 1:length(変数),
      ラベル =
        map_chr(
          data,
          \(data) {
            label <- pluck(data, attr_getter('label'))
            if(!is_null(label)) label else NA
          }
        )
    )
  }
}
