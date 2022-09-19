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
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom rlang as_label
#' @importFrom dplyr pull
#' @importFrom dplyr as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unite
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom stringr str_interp
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_unique
#' @importFrom janitor tabyl
#' @importFrom janitor untabyl
#' @importFrom janitor chisq.test
#' @importFrom janitor adorn_totals
#' @importFrom janitor adorn_percentages
#' @importFrom vcd assocstats
#' @importFrom gt gt
#' @importFrom gt tab_spanner
#' @importFrom gt tab_source_note
#'
#'
#'
#' @export
#'

my_cross <- function(.data, .x, .y, cramer = TRUE, p.value = TRUE, adjres = FALSE){
  .x <- enquo(.x)
  .y <- enquo(.y)

  .contents_.y <- pull(.data, !!.y)
  if(any(class(.contents_.y) == 'factor')){
    .contents_.y <-
      fct_explicit_na(.contents_.y, na_level = 'NA_') %>%
      fct_unique() %>%
      as.character()
  } else {
    .contents_.y <-
      factor(.contents_.y) %>%
      fct_explicit_na(na_level = 'NA_') %>%
      fct_unique() %>%
      as.character()
  }

  .tabyl <- tabyl(.data, !!.x, !!.y)

  N <-
    .tabyl %>%
    adorn_totals(where = c('row', 'col')) %>%
    as_tibble() %>%
    pull(Total) %>%
    # 合計のところの括弧
    str_c('（', ., '）')

  if(any(is.na(select(.data, !!.x, !!.y)))){
    .p.value <- NA
    .cramer <- NA
  } else {
    .p.value <-
      chisq.test(.tabyl) %>%
      .$p.value

    .cramer <-
      .tabyl %>%
      untabyl() %>%
      select(-1) %>%
      as.matrix() %>%
      assocstats() %>%
      .$cramer
  }

  .crosstab_raw <-
    .tabyl %>%
    adorn_totals(where = c('row', 'col')) %>%
    adorn_percentages(denominator = 'row') %>%
    as_tibble() %>%
    mutate(across(.cols = 1, .fns = ~{replace_na(., replace = 'NA_')}
    ))

  if(adjres == TRUE & !is.na(.p.value)) {
    .adjres <-
      chisq.test(.tabyl) %>%
      .$stdres %>%
      pivot_longer(cols = -1, names_to = 'name', values_to = 'adjres')

    .crosstab_raw <-
      pivot_longer(.crosstab_raw, cols = -1, names_to = 'name', values_to = 'percent')

    .crosstab_raw <-
      left_join(.crosstab_raw, .adjres, by = c(as_name(.x), 'name')) %>%
      mutate(
        p.value =
          abs(adjres) %>%
          pnorm(lower.tail = FALSE) %>%
          `*`(2),
        percent = map_chr(percent, ~str_interp('$[.1f]{.*100}'))
      ) %>%
      select(-adjres) %>%
      kamaken::p_star(p.value) %>%
      unite(col = 'percent', percent:p.value, sep = '') %>%
      pivot_wider(names_from = name, values_from = percent)
  } else {
    .crosstab_raw <-
      .crosstab_raw %>%
      mutate(across(where(is.numeric), ~map_chr(., ~str_interp('$[.1f]{.*100}')))
      )
  }

  .crosstab_raw <-
    .crosstab_raw %>%
    mutate(across(.cols = 1, .fns = ~str_c(., '（％）'))) %>%
    mutate(N = N)

  # gtによる整形
  .crosstab_gt <-
    gt(.crosstab_raw) %>%
    tab_spanner(label = as_label(.y),
                columns = .contents_.y)
  if(cramer == TRUE & p.value == TRUE){
    .crosstab_gt <- tab_source_note(.crosstab_gt,
                                    str_interp("Cramer's V = $[.3f]{.cramer}, chisq.test: p = $[.4f]{.p.value}"))
  } else if(cramer == TRUE & p.value == FALSE) {
    .crosstab_gt <- tab_source_note(.crosstab_gt,
                                    str_interp("Cramer's V = $[.3f]{.cramer}"))
  } else if(cramer == FALSE & p.value == TRUE) {
    .crosstab_gt <- tab_source_note(.crosstab_gt,
                                    str_interp("chisq.test: p = $[.4f]{.p.value}"))
  }

  return(.crosstab_gt)
}
