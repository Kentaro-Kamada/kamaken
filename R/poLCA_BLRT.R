poLCA_BLRT <- function(.poLCA_result_table, boot_size, maxiter = 6000, nrep = 3) {
  base_table <-
    .poLCA_result_table %>%
    dplyr::select(model.no, model, glance) %>%
    tidyr::unnest(glance) %>%
    dplyr::select(model.no, model, df.residual, logLik:g.squared) %>%
    dplyr::mutate(
      モデル対比 = stringr::str_c(dplyr::lag(model.no), ' vs ', model.no),
      # deviance差を算出（最終的な検定に用いる）
      delta_deviance = dplyr::lag(g.squared) - g.squared
    )

  # modelから条件付応答確率，クラス割合，サンプルサイズを抜き出す
  result <-
    dplyr::mutate(base_table,
                  probs = purrr::map(model, purrr::pluck, 'probs') %>% dplyr::lag(),
                  P = purrr::map(model, purrr::pluck, 'P') %>% dplyr::lag(),
                  N = purrr::map_dbl(model, purrr::pluck, 'N') %>% dplyr::lag(),
    ) %>%
    dplyr::filter(model.no != min(model.no)) %>%
    dplyr::select(!(model:g.squared))

  # 並列化の準備
  future::plan(strategy = future::multisession)

  # パラメトリックブートストラップ
  result <-
    result %>%
    dplyr::mutate(
      bootstrap =
        purrr::pmap(list(N, probs, P),
                    ~{dplyr::tibble(id = 1:boot_size,
                                    N2 = ..1,
                                    probs2 = list(..2),
                                    P2 = list(..3))
                    })) %>%
    dplyr::mutate(
      bootstrap =
        furrr::future_map(bootstrap, .options = furrr::furrr_options(seed = TRUE),
                          ~{dplyr::mutate(.,
                                          dat = furrr::future_pmap(list(N2, probs2, P2),
                                                                   .options = furrr::furrr_options(seed = TRUE),
                                                                   ~{poLCA::poLCA.simdata(N = ..1, probs = ..2, P = ..3) %>%
                                                                       .$dat}
                                          )) %>%
                              dplyr::select(id, dat)}
        )) %>%
    dplyr::select(-probs, -P, -N) %>%
    # ブートストラップサンプルに対して，クラス数kとクラス数k + 1の潜在クラス分析を実行
    dplyr::mutate(
      bootstrap =
        purrr::map2(model.no, bootstrap,
                    ~{dplyr::mutate(.y,
                                    model.no2 = .x,
                                    deviance_null =
                                      purrr::map2_dbl(dat, model.no2,
                                                      ~{poLCA::poLCA(formula = as.matrix(.x) ~ 1, data = .x,
                                                                     nclass = .y - 1, maxiter = maxiter, nrep = nrep,
                                                                     verbose = F) %>% .$Gsq}),
                                    deviance_alt =
                                      purrr::map2_dbl(dat, model.no2,
                                                      ~{poLCA::poLCA(formula = as.matrix(.x) ~ 1, data = .x,
                                                                     nclass = .y, maxiter = maxiter, nrep = nrep,
                                                                     verbose = F) %>% .$Gsq}),
                                    delta_deviance_boot = deviance_null - deviance_alt)
                    }
        )) %>%
    # p値の算出
    tidyr::unnest(bootstrap) %>%
    dplyr::group_by(モデル対比) %>%
    dplyr::summarise(p.value = sum(delta_deviance < delta_deviance_boot)/boot_size)

  future::plan(strategy = future::sequential)

  dplyr::left_join(base_table, result, by = 'モデル対比')
}

# boot_size = 1000で46分
