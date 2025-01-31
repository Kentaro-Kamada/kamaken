#' Goodness of Fit of LCA Models with `poLCA_result`
#'
#'
#' @importFrom dplyr select filter mutate group_by summarise left_join tibble lag
#' @importFrom tidyr nest unnest
#' @importFrom stringr str_c
#' @importFrom purrr map map2 map_dbl map2_dbl pmap pluck
#' @importFrom furrr future_map future_map2 future_map_dbl future_map2_dbl future_pmap furrr_options
#' @importFrom future plan availableCores
#' @importFrom poLCA poLCA poLCA.simdata
#'
#' @export
#'

poLCA_BLRT <- function(
    .poLCA_result_table,
    boot_size = F,
    maxiter = 6000,
    nrep = 3,
    seed = 123,
    workers = future::availableCores()
) {
  if (!inherits(.poLCA_result_table, "poLCA_result")) stop(".poLCA_result_table must be an object exported from `poLCA_result()`")
  if (!boot_size) {
    .poLCA_result_table |>
      select(model.no, model, glance) |>
      unnest(glance) |>
      select(model.no, model, df.residual, logLik:g.squared)
  }
  else {
    base_table <-
      .poLCA_result_table |>
      select(model.no, model, glance) |>
      unnest(glance) |>
      select(model.no, model, df.residual, logLik:g.squared) |>
      mutate(
        モデル対比 = str_c(lag(model.no), " vs ", model.no),
        # deviance差を算出（最終的な検定に用いる）
        delta_deviance = lag(g.squared) - g.squared
      )

    # modelから条件付応答確率，クラス割合，サンプルサイズを抜き出す
    result <-
      base_table |>
      mutate(
        probs = map(model, pluck, "probs") |> lag(),
        P = map(model, pluck, "P") |> lag(),
        N = map_dbl(model, pluck, "N") |> lag(),
      ) |>
      filter(model.no != min(model.no)) |>
      select(!(model:g.squared))

    # パラメトリックブートストラップ
    bootsample <-
      result |>
      mutate(
        bootstrap = pmap(
          list(N, probs, P),
          \(N, probs, P)
          # データの生成
          map(
            1:boot_size,
            \(i) poLCA::poLCA.simdata(N = N, probs = probs, P = P) |>
              pluck("dat")
          )
        )
      ) |>
      select(!c(probs, P, N)) |>
      unnest(bootstrap)

    # 並列化の準備
    future::plan(strategy = "multisession", workers = workers)

    # ブートストラップサンプルに対して，クラス数kとクラス数k + 1の潜在クラス分析を実行
    boot_result <-
      bootsample |>
      mutate(
        deviance_null = future_map2_dbl(
          bootstrap, model.no, .options = furrr_options(seed = seed),
          \(bootstrap, model.no)
          poLCA(
            formula = as.matrix(bootstrap) ~ 1,
            data = bootstrap,
            nclass = model.no - 1,
            maxiter = maxiter,
            nrep = nrep,
            verbose = F
          ) |> pluck("Gsq")
        ),
        deviance_alt = future_map2_dbl(
          bootstrap, model.no, .options = furrr_options(seed = seed),
          \(bootstrap, model.no)
          poLCA(
            formula = as.matrix(bootstrap) ~ 1,
            data = bootstrap,
            nclass = model.no,
            maxiter = maxiter,
            nrep = nrep,
            verbose = F
          ) |> pluck("Gsq")
        ),
        delta_deviance_boot = deviance_null - deviance_alt
      ) |>
      # p値の算出
      group_by(モデル対比) |>
      summarise(p.value = (sum(delta_deviance <= delta_deviance_boot) + 1) / (boot_size + 1))

    future::plan(strategy = "sequential")

    left_join(base_table, boot_result, by = "モデル対比")
  }
}



# carcinoma, nclass = 2:5, boot_size = 1000で4.5分（M1 Mac）
