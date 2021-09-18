# Bootstrap Likelihood Ratio Test
#'
#' Bootstrap Likelihood Ratio Test with `poLCA`
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble
#' @importFrom dplyr lag
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_dbl
#' @importFrom purrr pmap
#' @importFrom purrr pluck
#' @importFrom furrr future_map
#' @importFrom furrr future_map2
#' @importFrom furrr future_map_dbl
#' @importFrom furrr future_map2_dbl
#' @importFrom furrr future_pmap
#' @importFrom furrr furrr_options
#' @importFrom poLCA poLCA
#' @importFrom poLCA poLCA.simdata
#'
#' @export
#'

poLCA_BLRT <- function(.poLCA_result_table, boot_size = F, maxiter = 6000, nrep = 3) {
  if(!boot_size) {
    .poLCA_result_table %>%
      select(model.no, model, glance) %>%
      unnest(glance) %>%
      select(model.no, model, df.residual, logLik:g.squared)
  } else {
  base_table <-
    .poLCA_result_table %>%
    select(model.no, model, glance) %>%
    unnest(glance) %>%
    select(model.no, model, df.residual, logLik:g.squared) %>%
    mutate(
      モデル対比 = str_c(lag(model.no), ' vs ', model.no),
      # deviance差を算出（最終的な検定に用いる）
      delta_deviance = lag(g.squared) - g.squared
    )

  # modelから条件付応答確率，クラス割合，サンプルサイズを抜き出す
  result <-
    mutate(base_table,
           probs = map(model, pluck, 'probs') %>% lag(),
           P = map(model, pluck, 'P') %>% lag(),
           N = map_dbl(model, pluck, 'N') %>% lag(),
    ) %>%
    filter(model.no != min(model.no)) %>%
    select(!(model:g.squared))

  # 並列化の準備
  future::plan(strategy = future::multisession)

  # パラメトリックブートストラップ
  result <-
    result %>%
    mutate(
      bootstrap =
        pmap(list(N, probs, P),
             ~{tibble(id = 1:boot_size,
                      N2 = ..1,
                      probs2 = list(..2),
                      P2 = list(..3))
             })) %>%
    mutate(
      bootstrap =
        future_map(bootstrap, .options = furrr_options(seed = TRUE),
                   ~{mutate(.,
                            dat = future_pmap(list(N2, probs2, P2),
                                              .options = furrr_options(seed = TRUE),
                                              ~{poLCA.simdata(N = ..1, probs = ..2,
                                                              P = ..3) %>% .$dat})
                   ) %>% select(id, dat)}
        )) %>%
    select(-probs, -P, -N) %>%
    # ブートストラップサンプルに対して，クラス数kとクラス数k + 1の潜在クラス分析を実行
    mutate(
      bootstrap =
        future_map2(model.no, bootstrap, .options = furrr_options(seed = TRUE),
             ~{mutate(.y,
                      model.no2 = .x,
                      deviance_null =
                        future_map2_dbl(dat, model.no2,
                                        .options = furrr_options(seed = TRUE),
                                        ~{poLCA(formula = as.matrix(.x) ~ 1, data = .x,
                                                nclass = .y - 1, maxiter = maxiter,
                                                nrep = nrep,
                                                verbose = F) %>% .$Gsq}),
                      deviance_alt =
                        future_map2_dbl(dat, model.no2,
                                        .options = furrr_options(seed = TRUE),
                                        ~{poLCA(formula = as.matrix(.x) ~ 1, data = .x,
                                                nclass = .y, maxiter = maxiter,
                                                nrep = nrep,
                                                verbose = F) %>% .$Gsq}),
                      delta_deviance_boot = deviance_null - deviance_alt)
             }
        )) %>%
    # p値の算出
    unnest(bootstrap) %>%
    group_by(モデル対比) %>%
    summarise(p.value = sum(delta_deviance < delta_deviance_boot)/boot_size)

  future::plan(strategy = future::sequential)

  left_join(base_table, result, by = 'モデル対比')
  }
}

# carcinoma, nclass = 2:4, boot_size = 1000で12分
