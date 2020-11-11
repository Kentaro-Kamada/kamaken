my_cross <- function(.data, .x, .y){
  .x <- rlang::enquo(.x)
  .y <- rlang::enquo(.y)

  .contents_.y <- dplyr::pull(.data, !!.y)
  if(any(class(.contents_.y) == 'factor')){
    .contents_.y <-
      forcats::fct_explicit_na(.contents_.y, na_level = 'NA_') %>%
      forcats::fct_unique() %>%
      as.character()
  } else {
    .contents_.y <-
      factor(.contents_.y) %>%
      forcats::fct_explicit_na(na_level = 'NA_') %>%
      forcats::fct_unique() %>%
      as.character()
  }

  .tabyl <- janitor::tabyl(.data, !!.x, !!.y)

  N <-
    .tabyl %>%
    janitor::adorn_totals(where = c('row', 'col')) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(Total) %>%
    # 合計のところの括弧
    stringr::str_c('（', ., '）')

  if(any(is.na(dplyr::select(.data, !!.x, !!.y)))){
    .p.value <- NA
    .cramer <- NA
  } else {
    .p.value <-
      janitor::chisq.test(.tabyl) %>%
      .$p.value

    .cramer <-
      .tabyl %>%
      janitor::untabyl() %>%
      dplyr::select(-1) %>%
      as.matrix() %>%
      vcd::assocstats() %>%
      .$cramer
  }

  .crosstab_raw <-
    .tabyl %>%
    janitor::adorn_totals(where = c('row', 'col')) %>%
    janitor::adorn_percentages(denominator = 'row') %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(dplyr::across(.cols = 1,
                                .fns = ~{tidyr::replace_na(., replace = 'NA_')}
    )) %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric),
                                .fns = ~{round(.*100, digits = 1)}
    )) %>%
    # パーセント記号
    dplyr::mutate(dplyr::across(.cols = 1,
                                .fns = ~{stringr::str_c(., '（％）')}
    )) %>%
    dplyr::mutate(N = N)

  # gtによる整形
  .crosstab_gt <-
    gt::gt(.crosstab_raw) %>%
    gt::tab_spanner(label = rlang::as_label(.y),
                    columns = .contents_.y) %>%
    gt::tab_source_note(stringr::str_interp("Cramer's V = $[.3f]{.cramer}   p = $[.4f]{.p.value}"))

  return(.crosstab_gt)
}
