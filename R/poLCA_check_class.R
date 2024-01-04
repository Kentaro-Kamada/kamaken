#' format a result of latent class analysis with `poLCA_result`
#'
#' format a result of latent class analysis with `poLCA_result`
#'
#' @param .poLCA_result_table an object exported from `poLCA_result()`
#' @param nclass number of latent classes to check class property
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter tibble pull transmute row_number mutate bind_rows select
#' @importFrom tidyr unnest pivot_wider
#' @importFrom purrr list_flatten pluck
#'
#' @export
#'


poLCA_check_class <- function(.poLCA_result_table, nclass){
  if(!inherits(.poLCA_result_table, 'poLCA_result')) stop('.poLCA_result_table must be an object exported from `poLCA_result()`')
  .poLCA <- filter(.poLCA_result_table, model.no == nclass)
  class_prop <-
    tibble(
      estimate =
        pull(.poLCA, model) %>%
        pluck(1, 'P')
    ) %>%
    transmute(
      variable = 'クラス構成割合',
      class = row_number(),
      outcome = NA,
      estimate
    )

  lca_output <-
    unnest(.poLCA, tidy) %>%
    select(variable:estimate) %>%
    bind_rows(class_prop, .) %>%
    pivot_wider(names_from = class, values_from = estimate)

  return(lca_output)

}
