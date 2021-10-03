# useful short cut for `poLCA::poLCA`
#'
#' useful short cut for `poLCA::poLCA`. you get a result table for reporting with the other functions like `poLCA_check_class` or `poLCA_BLRT`.
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr pluck
#' @importFrom poLCA poLCA
#' @importFrom poLCA poLCA.reorder
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom broom augment
#'
#' @export
#'

poLCA_result <- function(formula, data, nclass, maxiter = 5000, nrep = 1,
                         reorder_with = 'class prop', reorder_outcome = 1,
                         reorder_decreasing = TRUE) {

  if(reorder_with == 'class prop') {
    .result_table <-
      tibble(
        model.no = nclass,
        model = map(model.no,
                    ~{poLCA(formula = formula, data = data,
                            maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)}
        ),
        probs_start = map(model,
                          ~{poLCA.reorder(.$probs.start,
                                          purrr::pluck(., 'P') %>%
                                            order(decreasing = reorder_decreasing))}
        )
      )
  } else {
    .result_table <-
      tibble(
        model.no = nclass,
        model = map(model.no,
                    ~{poLCA(formula = formula, data = data,
                            maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)}
        ),
        probs_start = map(model,
                          ~{poLCA.reorder(.$probs.start,
                                          purrr::pluck(., 'probs', reorder_with) %>%
                                            .[,reorder_outcome] %>%
                                            as.numeric() %>%
                                            order(decreasing = reorder_decreasing))}
        )
      )

  }

  .result_table <-
    mutate(.result_table,
           model = map2(model.no, probs_start,
                        ~{poLCA(formula = formula,
                                data = data, maxiter = maxiter, nrep = 1,
                                nclass = .x, probs.start = .y)
                        }),
           tidy = map(model, broom::tidy),
           glance = map(model, broom::glance),
           augment = map(model, broom::augment)
    )

  return(.result_table)

}
