# Create a list of variable labels from SPSS or Stata data
#'
#' Create a list of variable labels from SPSS or Stata data
#'
#' @importFrom dplyr tibble
#' @importFrom purrr map_chr
#' @importFrom purrr pluck
#' @importFrom purrr attr_getter
#' @importFrom purrr is_null
#'
#' @export
#'

haven_variable_label <- function(data) {
  if(!(any(class(data) %in% c('data.frame', 'tbl_df')))) stop('`data` must be `data.frame` or `tibble`')
  else {
    tibble(
      変数 = names(data),
      位置 = 1:length(変数),
      ラベル =
        map_chr(
          data,
          ~{label <- pluck(., attr_getter('label'))
          if(!is_null(label)) label else NA_character_}
        )
    )
  }
}
