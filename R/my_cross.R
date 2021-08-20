# Cross tabulations for categorical variables
#'
#' Generate a contingency table.
#'
#' @param .data input a tibble or data.frame
#' @param .x first variable which will be row names of the table
#' @param .y second variable which will be columns of the table
#' @param cramer if `TRUE`, calculates the value of cramer's V. default is `TRUE`
#' @param p.value if `TRUE`, calculates p value of chi-squared test. default is `TRUE`
#' @param adjres if `TRUE`, calculates adjusted residual and shows the results of chi-square residual tests. default is `FALSE`
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
my_cross <- function(.data, .x, .y, cramer = TRUE, p.value = TRUE, adjres = FALSE){
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
    ))

  if(adjres == TRUE & !is.na(.p.value)) {
    .adjres <-
      janitor::chisq.test(.tabyl) %>%
      .$stdres %>%
      tidyr::pivot_longer(cols = -1, names_to = 'name', values_to = 'adjres')

    .crosstab_raw <-
      tidyr::pivot_longer(.crosstab_raw, cols = -1, names_to = 'name', values_to = 'percent')

    .crosstab_raw <-
      dplyr::left_join(.crosstab_raw, .adjres, by = c(rlang::as_name(.x), 'name')) %>%
      dplyr::mutate(
        p.value =
          abs(adjres) %>%
          pnorm(lower.tail = FALSE) %>%
          `*`(2),
        percent = purrr::map_chr(percent,
                                 ~{stringr::str_interp('$[.1f]{.*100}')}
        )
      ) %>%
      dplyr::select(-adjres) %>%
      kamaken::p_star(p.value) %>%
      tidyr::unite(col = 'percent', percent:p.value, sep = '') %>%
      tidyr::pivot_wider(names_from = name, values_from = percent)
  } else {
    .crosstab_raw <-
      .crosstab_raw %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric),
                      ~{purrr::map_chr(.,
                                       ~{stringr::str_interp('$[.1f]{.*100}')})
                      })
      )
  }

  .crosstab_raw <-
    .crosstab_raw %>%
    dplyr::mutate(dplyr::across(.cols = 1,
                                .fns = ~{stringr::str_c(., '（％）')}
    )) %>%
    dplyr::mutate(N = N)

  # gtによる整形
  .crosstab_gt <-
    gt::gt(.crosstab_raw) %>%
    gt::tab_spanner(label = rlang::as_label(.y),
                    columns = .contents_.y)
  if(cramer == TRUE & p.value == TRUE){
    .crosstab_gt <- gt::tab_source_note(.crosstab_gt,
                                        stringr::str_interp("Cramer's V = $[.3f]{.cramer},
                                                            chisq.test: p = $[.4f]{.p.value}"))
  } else if(cramer == TRUE & p.value == FALSE) {
    .crosstab_gt <- gt::tab_source_note(.crosstab_gt,
                                        stringr::str_interp("Cramer's V = $[.3f]{.cramer}"))
  } else if(cramer == FALSE & p.value == TRUE) {
    .crosstab_gt <- gt::tab_source_note(.crosstab_gt,
                                        stringr::str_interp("chisq.test: p = $[.4f]{.p.value}"))
  }

  return(.crosstab_gt)
}
