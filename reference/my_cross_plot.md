# Generate a plot converted from a contingency table.

Generate a plot converted from a contingency table.

## Usage

``` r
my_cross_plot(
  .data,
  .x,
  .y,
  strata = NULL,
  row_percent = TRUE,
  text_size = 4,
  p.value = FALSE
)
```

## Arguments

- .data:

  input a tibble or data.frame

- .x:

  first variable which will be row names of the table

- .y:

  second variable which will be columns of the table

- row_percent:

  if `TRUE`, shows row percent with text, default is `TRUE`

- text_size:

  size of text

- p.value:

  if `TRUE`, calculates p value of chi-squared test. default is `TRUE`
