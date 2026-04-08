# Estimate the causal effect by G-computation with jackknife variance estimation.

Estimate the causal effect by G-computation with jackknife variance
estimation.

## Usage

``` r
gcomp_jack(
  .data,
  .outcome,
  .treatment,
  .formula_rhs,
  .estimand = c("cfmean", "ATE"),
  .weights,
  .repweights,
  .type = c("JK1", "JK2"),
  .by = NULL
)
```

## Arguments

- .data:

  A tibble that contains the data.

- .outcome:

  The name of the outcome variable.

- .treatment:

  The name of the treatment binary variable, only allowed to be 0 or 1.

- .formula_rhs:

  The right-hand side of the regression formula.

- .estimand:

  Specify the estimand. 'cfmean' or 'ATE'.

- .weights:

  The name of the sampling weights.

- .repweights:

  The name of the replicate weights.

- .type:

  The variation of jackknife variance estimation. 'JK1' or 'JK2'.

- .by:

  The variable to be used for heterogeneity analysis. If NULL, the
  entire population is used.
