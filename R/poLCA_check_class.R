# format a result of latent class analysis
#'
#' format a result of latent class analysis
#'
#' @export
#'


poLCA_check_class <- function(.poLCA_result_table, nclass){
  .poLCA <- dplyr::filter(.poLCA_result_table, .data$model.no == nclass)
  class_prop <-
    dplyr::tibble(
      estimate =
        dplyr::pull(.poLCA, .data$model) %>%
        purrr::flatten() %>% .$P
    ) %>%
    dplyr::transmute(
      variable = 'クラス構成割合',
      class = dplyr::row_number(),
      outcome = NA_real_,
      estimate
    )

  lca_output <-
    tidyr::unnest(.poLCA, .data$tidy) %>%
    dplyr::select(variable:estimate) %>%
    dplyr::bind_rows(class_prop, .) %>%
    dplyr::mutate(estimate = round(estimate, 3)) %>%
    tidyr::pivot_wider(names_from = class, values_from = estimate)

  return(lca_output)

}
