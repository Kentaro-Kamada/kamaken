poLCA_logit_tidy <- function(.poLCA_result_table) {
  dplyr::inner_join(
    .poLCA_result_table %>%
      dplyr::pull(model) %>%
      purrr::pluck(1, 'coeff') %>%
      dplyr::as_tibble(rownames = 'variables', .name_repair = 'unique') %>%
      tidyr::pivot_longer(cols = -variables, names_to = 'group', values_to = 'Coef.'),
    .poLCA_result_table %>%
      dplyr::pull(model) %>%
      purrr::pluck(1, 'coeff.se') %>%
      dplyr::as_tibble(rownames = 'variables', .name_repair = 'unique') %>%
      tidyr::pivot_longer(cols = -variables, names_to = 'group', values_to = 'S.E.'),
    by = c('variables', 'group')
  ) %>%
    dplyr::mutate(
      t.value = Coef./S.E.,
      df = .poLCA_result_table %>%
        dplyr::pull(model) %>%
        purrr::pluck(1, 'resid.df'),
      p.value = pt(abs(t.value), df = df, lower.tail = FALSE)*2
    )
}
