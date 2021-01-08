poLCA_result <- function(formula, data, nclass, maxiter = 5000, nrep = 1,
                         reorder_with = 'class prop', reorder_outcome = 1,
                         reorder_decreasing = TRUE) {

  if(reorder_with == 'class prop') {
    .result_table <-
      dplyr::tibble(
        model.no = nclass,
        model = purrr::map(model.no,
                           ~{poLCA::poLCA(formula = formula, data = data,
                                          maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)}
        ),
        probs_start = purrr::map(model,
                                 ~{poLCA::poLCA.reorder(.$probs.start,
                                                        purrr::pluck(., 'P') %>%
                                                          order(decreasing = reorder_decreasing))}
        )
      )
  } else {
    .result_table <-
      dplyr::tibble(
        model.no = nclass,
        model = purrr::map(model.no,
                           ~{poLCA::poLCA(formula = formula, data = data,
                                          maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)}
        ),
        probs_start = purrr::map(model,
                                 ~{poLCA::poLCA.reorder(.$probs.start,
                                                        purrr::pluck(., 'probs', reorder_with) %>%
                                                          .[,reorder_outcome] %>%
                                                          as.numeric() %>%
                                                          order(decreasing = reorder_decreasing))}
        )
      )

  }

  .result_table <-
    dplyr::mutate(.result_table,
                  model = purrr::map2(model.no, probs_start,
                                      ~{poLCA::poLCA(formula = formula,
                                                     data = data, maxiter = maxiter, nrep = 1,
                                                     nclass = .x, probs.start = .y)
                                      }),
                  tidy = purrr::map(model, broom::tidy),
                  glance = purrr::map(model, broom::glance),
                  augment = purrr::map(model, broom::augment)
    )

  return(.result_table)

}
