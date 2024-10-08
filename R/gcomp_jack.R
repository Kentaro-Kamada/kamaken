#'
#' Estimate the causal effect by G-computation with jackknife variance estimation.
#'
#' @param .data A tibble that contains the data.
#' @param .outcome The name of the outcome variable.
#' @param .treatment The name of the treatment binary variable, only allowed to be 0 or 1.
#' @param .formula_rhs The right-hand side of the regression formula.
#' @param .estimand Specify the estimand. 'cfmean' or 'ATE'.
#' @param .weights The name of the sampling weights.
#' @param .repweights The name of the replicate weights.
#' @param .type The variation of jackknife variance estimation. 'JK1' or 'JK2'.
#' @param .by The variable to be used for heterogeneity analysis. If NULL, the entire population is used.
#'
#' @importFrom rlang enquo ensym f_lhs<- new_formula
#' @importFrom dplyr select mutate filter rename group_by summarise bind_rows left_join case_when pick
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom broom augment
#' @importFrom fixest feols
#' @importFrom furrr future_map future_options
#'
#' @export
#'


gcomp_jack <-
  function(.data, .outcome, .treatment, .formula_rhs, .estimand = c('cfmean', 'ATE'), .weights, .repweights, .type = c('JK1', 'JK2'), .by = NULL) {
    .treatment <- enquo(.treatment)
    .weights <- enquo(.weights)
    .repweights <- select(.data, {{.repweights}}) |> names()
    .by <- enquo(.by)
    # scale parameterの設定
    scale <- case_when(
      .type == 'JK1' ~ (length(.repweights) - 1) / length(.repweights),
      .type == 'JK2' ~ 1
    )
    # formulaの左辺に.outcomeを追加
    .formula <- `f_lhs<-`(.formula_rhs, ensym(.outcome))

    # OLS推定
    fit <-
      feols(
        .formula,
        data = .data,
        combine.quick = FALSE,
        weights = .weights # quosureはformulaでもある
      )
    # 点推定値
    res_point_estimate <-
      bind_rows(
        augment(fit, newdata = .data |> mutate(!!.treatment := 0)),
        augment(fit, newdata = .data |> mutate(!!.treatment := 1)),
      ) |>
      group_by(pick(!!.treatment, !!.by)) |>
      summarise(
        estimate = weighted.mean(.fitted, !!.weights),
        .groups = 'drop'
      ) |>
      mutate(estimand = 'cfmean')
    if (.estimand == 'ATE') {
      res_point_estimate <-
        res_point_estimate |>
        mutate(estimand = 'ATE') |>
        pivot_wider(names_from = !!.treatment, values_from = estimate) |>
        mutate(estimate = `1` - `0`) |>
        select(!c(`0`, `1`))
    }

    # 各replicate weightを使ってOLS推定し、jackknifeサンプルにおける点推定値を計算する
    fit_jack <-
      future_map(
        .repweights, .progress = TRUE, .options = furrr_options(seed = TRUE),
        \(w) {
          gc()
          # 文字列をシンボル化
          w <- ensym(w)
          # weightが0のデータを削除
          df_jack <- .data |> filter(!!w != 0)
          # OLSで推定
          fit <-
            feols(
              .formula,
              data = df_jack,
              weights = new_formula(lhs = NULL, rhs = w),
              combine.quick = FALSE
            )
          # Y^1とY^0を計算
          res_point_estimate_jack <-
            bind_rows(
              augment(fit, newdata = df_jack |> mutate(!!.treatment := 0)),
              augment(fit, newdata = df_jack |> mutate(!!.treatment := 1)),
            ) |>
            group_by(pick(!!.treatment, !!.by)) |>
            summarise(
              estimate = weighted.mean(.fitted, !!w),
              .groups = 'drop'
            ) |>
            mutate(estimand = 'cfmean')
          if (.estimand == 'ATE') {
            res_point_estimate_jack <-
              res_point_estimate_jack |>
              mutate(estimand = 'ATE') |>
              pivot_wider(names_from = !!.treatment, values_from = estimate) |>
              mutate(estimate = `1` - `0`) |>
              select(!c(`0`, `1`))
          }
          res_point_estimate_jack
        }
      )
    # 結果の統合
    results <-
      fit_jack |>
      bind_rows(.id = 'no') |>
      rename(estimate_jack = estimate) |>
      # 点推定値をくっつける
      left_join(res_point_estimate |> rename(estimate_all = estimate))

    # jackknife標準誤差の計算
    if (.estimand == 'cfmean') {
      results <-
        results |>
        group_by(pick(estimand, !!.treatment, !!.by)) |>
        summarise(
          estimate = mean(estimate_all),
          std.error = sqrt(scale * sum((estimate_all - estimate_jack)^2, na.rm = TRUE)),
          .groups = 'drop'
        )
    }
    else if (.estimand == 'ATE') {
      results <-
        results |>
        group_by(pick(estimand, !!.by)) |>
        summarise(
          estimate = mean(estimate_all),
          std.error = sqrt(scale * sum((estimate_all - estimate_jack)^2, na.rm = TRUE)),
          .groups = 'drop'
        )
    }
    results
  }


