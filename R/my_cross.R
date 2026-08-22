#' Cross tabulations for categorical variables
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
#' @importFrom stringr str_c
#' @importFrom stringr str_glue
#' @importFrom forcats fct_na_value_to_level
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_unique
#' @importFrom forcats as_factor
#' @importFrom janitor tabyl
#' @importFrom janitor untabyl
#' @importFrom janitor chisq.test
#' @importFrom janitor adorn_totals
#' @importFrom janitor adorn_percentages
#' @importFrom DescTools CramerV
#' @importFrom gt gt
#' @importFrom gt cols_align
#' @importFrom gt tab_spanner
#' @importFrom gt tab_source_note
#'
#'
#'
#' @export
#'

my_cross <- function(.data, .x, .y, cramer = TRUE, p.value = TRUE, adjres = FALSE) {
  .x <- enquo(.x)
  .y <- enquo(.y)

  .y_levels <-
    .data |>
    pull(!!.y) |>
    as_factor() |>
    fct_na_value_to_level(level = "NA_") |>
    fct_drop() |>
    fct_unique() |>
    as.character()

  .tabyl <- tabyl(.data, !!.x, !!.y)

  .tabyl_with_totals <-
    .tabyl |>
    adorn_totals(where = c("row", "col"))

  .n <- pull(.tabyl_with_totals, Total)

  # 欠損値の有無を確認
  .has_missing <- anyNA(select(.data, !!.x, !!.y))
  .chisq <- NULL
  .p_value <- NA_real_
  .cramer <- NA_real_

  # 欠損値がない場合にのみp値を計算
  if (!.has_missing && (isTRUE(p.value) || isTRUE(adjres))) {
    .chisq <- chisq.test(.tabyl)
    .p_value <- .chisq$p.value
  }

  # Cramer's Vを計算する
  if (!.has_missing && isTRUE(cramer)) {
    .cramer <-
      .tabyl |>
      untabyl() |>
      select(!(!!.x)) |>
      as.matrix() |>
      CramerV()
  }

  # パーセンテージを計算し、欠損値を"NA_"に置換する
  .crosstab_raw <-
    .tabyl_with_totals |>
    adorn_percentages(denominator = "row") |>
    as_tibble() |>
    mutate(!!.x := replace_na(!!.x, replace = "NA_"))

  # adjresがTRUEの場合、調整残差を計算し、p値を付加する
  if (isTRUE(adjres) && !is.null(.chisq)) {
    .adjres <-
      .chisq$stdres |>
      pivot_longer(cols = !(!!.x), names_to = "name", values_to = "adjres")

    .crosstab_raw <-
      .crosstab_raw |>
      pivot_longer(
        cols = !(!!.x),
        names_to = "name",
        values_to = "percent"
      ) |>
      left_join(.adjres, by = c(as_name(.x), "name")) |>
      mutate(
        p.value = 2 * pnorm(abs(adjres), lower.tail = FALSE),
        percent = scales::percent(percent, accuracy = 0.1)
      ) |>
      select(!adjres) |>
      kamaken::p_star(p.value) |>
      unite(col = "percent", percent:p.value, sep = "") |>
      pivot_wider(names_from = name, values_from = percent)
  } else {
    .crosstab_raw <-
      .crosstab_raw |>
      mutate(across(where(is.numeric), \(x) scales::percent(x, accuracy = 0.1)))
  }

  # 度数を付加する
  .crosstab_raw <- .crosstab_raw |> mutate(N = scales::number(.n, accuracy = 1, big.mark = ","))

  # gtによる整形
  .crosstab_gt <-
    .crosstab_raw |>
    gt() |>
    cols_align(align = "left", columns = !!.x) |>
    tab_spanner(label = as_label(.y), columns = .y_levels)

  # 注釈の作成
  .notes <- character()
  if (isTRUE(cramer)) {
    .notes <-
      c(
        .notes,
        str_glue(
          "Cramer's V = {scales::number(.cramer, accuracy = 0.001)}"
        )
      )
  }
  if (isTRUE(p.value)) {
    .notes <-
      c(
        .notes,
        str_glue(
          "Pearson's Chi-squared test: {scales::pvalue(.p_value, prefix = c('p < ', 'p = ', 'p > '))}"
        )
      )
  }

  if (length(.notes) > 0L) {
    .crosstab_gt <-
      .crosstab_gt |>
      tab_source_note(str_c(.notes, collapse = ", "))
  }

  .crosstab_gt
}
