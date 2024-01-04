#' Useful short cut for `poLCA::poLCA`.
#'
#' you get a result table for reporting with the other functions like `poLCA_check_class` or `poLCA_BLRT`.
#'
#' @param formula a formula object directly passed to `poLCA::poLCA`.
#' @param data a data frame.
#' @param nclass a numeric vector of number of latent class to estimate. For example, if you want to estimate 2 class model to 5 class model for comparing model fitting of them, input `nclass = 2:5`.
#' @param maxiter The maximum number of iterations through which the estimation algorithm will cycle.
#' @param nrep Number of times to estimate the model, using different values of probs.start. The default is one. Setting nrep>1 automates the search for the global—rather than just a local—maximum of the log-likelihood function. poLCA returns the parameter estimates corresponding to the model with the greatest log-likelihood.
#' @param reorder_with 'class_prop' or one variable name. If 'class_prop', the order of latent class is determined by the proportion of each class. If one variable name, the order of latent class is determined by the conditional probability of the variable. The default is 'class_prop'.
#' @param reorder_outcome a numeric value of a category of manifest variable. You can reorder latent classes with reference to the category of manifest variable. You must specify a manifest variable in `reorder_with` argment. The default is 1.
#'
#' @seealso [poLCA::poLCA()]
#'
#' @examples
#' # example data
#' data('carcinoma', package = 'poLCA')
#'
#' # estimate 2 class model to 3 class model
#' result <- poLCA_result(
#'   formula = as.matrix(carcinoma) ~ 1,
#'   data = carcinoma,
#'   nclass = 2:3,
#'   maxiter = 6000,
#'   nrep = 1,
#'   verbose = TRUE
#' )
#'
#' @importFrom dplyr tibble mutate
#' @importFrom purrr map map2 pluck
#' @importFrom poLCA poLCA poLCA.reorder
#' @importFrom broom tidy glance augment
#'
#' @export
#'

poLCA_result <- function(formula, data, nclass, maxiter = 5000, nrep = 1,
                         reorder_with = 'class prop', reorder_outcome = 1,
                         reorder_decreasing = TRUE, verbose = TRUE) {

  if(reorder_with == 'class prop') {
    .result_table <-
      tibble(
        model.no = nclass,
        model =
          map(model.no, .progress = T, ~{
            poLCA(formula = formula, data = data,
                  maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)
          }),
        probs_start =
          map(model, ~{
            poLCA.reorder(.$probs.start,
                          purrr::pluck(., 'P') %>%
                            order(decreasing = reorder_decreasing))
            })
      )
  } else {
    .result_table <-
      tibble(
        model.no = nclass,
        model =
          map(model.no, .progress = T, ~{
            poLCA(formula = formula, data = data,
                  maxiter = maxiter, nrep = nrep, nclass = ., verbose = FALSE)
            }),
        probs_start =
          map(model, ~{
            poLCA.reorder(
              probs = pluck(., 'probs.start'),
              o.new =
                pluck(., 'probs', reorder_with) %>%
                .[,reorder_outcome] %>%
                as.numeric() %>%
                order(decreasing = reorder_decreasing)
              )
            })
      )

  }

  .result_table <-
    .result_table %>%
    mutate(
      model =
        map2(
          model.no, probs_start, ~{
            poLCA(formula = formula,
                  data = data, maxiter = maxiter, nrep = 1,
                  nclass = .x, probs.start = .y, verbose = verbose)
          }),
      tidy = map(model, broom::tidy),
      glance = map(model, broom::glance),
      augment = map(model, broom::augment)
    )

  class(.result_table) <- c('poLCA_result', class(.result_table))
  return(.result_table)

}

