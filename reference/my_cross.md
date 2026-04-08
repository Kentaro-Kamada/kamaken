# Cross tabulations for categorical variables

Generate a contingency table.

## Usage

``` r
my_cross(.data, .x, .y, cramer = TRUE, p.value = TRUE, adjres = FALSE)
```

## Arguments

- .data:

  input a tibble or data.frame

- .x:

  first variable which will be row names of the table

- .y:

  second variable which will be columns of the table

- cramer:

  if `TRUE`, calculates the value of cramer's V. default is `TRUE`

- p.value:

  if `TRUE`, calculates p value of chi-squared test. default is `TRUE`

- adjres:

  if `TRUE`, calculates adjusted residual and shows the results of
  chi-square residual tests. default is `FALSE`
