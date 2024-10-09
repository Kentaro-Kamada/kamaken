#'
#' Integration of estimation results by multiple imputation method following rubin's rule.
#'
#' @param .tibble A tibble that contains the results of the estimation.
#' @param estimate The name of the column that contains the point estimates.
#' @param std.error The name of the column that contains the standard errors.
#' @param term When you want to integrate multiple parameters, specify the name of the columns that identifies the parameters. Allowed multiple column names like `c(a, b)`.
#'
#' @importFrom rlang enquo
#' @importFrom dplyr mutate summarise select n
#'
#' @return A tibble that contains the integrated results.
#'
#'
#' @export
#'


pool_rubin <- function(.tibble, estimate = estimate, std.error = std.error, term = NULL) {
  estimate <- enquo(estimate)
  std.error <- enquo(std.error)
  term <- enquo(term)
  .tibble |>
    mutate(variance__ = (!!std.error)^2) |>
    summarise(
      M = n(),
      estimate.combined = mean(!!estimate),
      Vw = mean(variance__),
      Vb = var(!!estimate),
      Vt = Vw + (1 + 1 / M) * Vb,
      SE.combined = sqrt(Vt),
      .by = !!term
    ) |>
    # 信頼区間計算
    mutate(
      conf.low = estimate.combined - qnorm(1 - .025)*SE.combined,
      conf.high = estimate.combined + qnorm(1 - .025)*SE.combined
    ) |>
    select(!!term, M, estimate = estimate.combined, std.error = SE.combined, conf.low, conf.high)
}


