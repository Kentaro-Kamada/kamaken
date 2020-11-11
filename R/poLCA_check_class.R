poLCA_check_class <- function(.poLCA){
  class_prop <-
    tibble(
      estimate =
        dplyr::pull(.poLCA, .data$model) %>%
        flatten() %>% .$P
    ) %>%
    transmute(
      variable = 'クラス構成割合',
      class = row_number(),
      outcome = NA_real_,
      estimate
    )

  lca_output <-
    unnest(.poLCA, .data$tidy) %>%
    select(variable:estimate) %>%
    bind_rows(class_prop, .) %>%
    mutate(estimate = round(estimate, 3)) %>%
    pivot_wider(names_from = class, values_from = estimate)

  return(lca_output)

}
