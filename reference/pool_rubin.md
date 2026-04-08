# Integration of estimation results by multiple imputation method following rubin's rule.

Integration of estimation results by multiple imputation method
following rubin's rule.

## Usage

``` r
pool_rubin(.tibble, estimate = estimate, std.error = std.error, term = NULL)
```

## Arguments

- .tibble:

  A tibble that contains the results of the estimation.

- estimate:

  The name of the column that contains the point estimates.

- std.error:

  The name of the column that contains the standard errors.

- term:

  When you want to integrate multiple parameters, specify the name of
  the columns that identifies the parameters. Allowed multiple column
  names like `c(a, b)`.

## Value

A tibble that contains the integrated results.
