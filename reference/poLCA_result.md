# Useful short cut for `poLCA::poLCA`.

you get a result table for reporting with the other functions like
`poLCA_check_class` or `poLCA_BLRT`.

## Usage

``` r
poLCA_result(
  formula,
  data,
  nclass,
  maxiter = 5000,
  nrep = 1,
  reorder_with = "class prop",
  reorder_outcome = 1,
  reorder_decreasing = TRUE,
  verbose = TRUE
)
```

## Arguments

- formula:

  a formula object directly passed to
  [`poLCA::poLCA`](https://rdrr.io/pkg/poLCA/man/poLCA.html).

- data:

  a data frame.

- nclass:

  a numeric vector of number of latent class to estimate. For example,
  if you want to estimate 2 class model to 5 class model for comparing
  model fitting of them, input `nclass = 2:5`.

- maxiter:

  The maximum number of iterations through which the estimation
  algorithm will cycle.

- nrep:

  Number of times to estimate the model, using different values of
  probs.start. The default is one. Setting nrep\>1 automates the search
  for the global—rather than just a local—maximum of the log-likelihood
  function. poLCA returns the parameter estimates corresponding to the
  model with the greatest log-likelihood.

- reorder_with:

  'class_prop' or one variable name. If 'class_prop', the order of
  latent class is determined by the proportion of each class. If one
  variable name, the order of latent class is determined by the
  conditional probability of the variable. The default is 'class_prop'.

- reorder_outcome:

  a numeric value of a category of manifest variable. You can reorder
  latent classes with reference to the category of manifest variable.
  You must specify a manifest variable in `reorder_with` argment. The
  default is 1.

## See also

[`poLCA::poLCA()`](https://rdrr.io/pkg/poLCA/man/poLCA.html)

## Examples

``` r
# example data
data('carcinoma', package = 'poLCA')

# estimate 2 class model to 3 class model
result <- poLCA_result(
  formula = as.matrix(carcinoma) ~ 1,
  data = carcinoma,
  nclass = 2:3,
  maxiter = 6000,
  nrep = 1,
  verbose = TRUE
)
#> Conditional item response (column) probabilities,
#>  by outcome variable, for each class (row) 
#>  
#> $A
#>            Pr(1)  Pr(2)
#> class 1:  0.0000 1.0000
#> class 2:  0.8835 0.1165
#> 
#> $B
#>            Pr(1)  Pr(2)
#> class 1:  0.0169 0.9831
#> class 2:  0.6456 0.3544
#> 
#> $C
#>            Pr(1)  Pr(2)
#> class 1:  0.2391 0.7609
#> class 2:  1.0000 0.0000
#> 
#> $D
#>            Pr(1)  Pr(2)
#> class 1:  0.4589 0.5411
#> class 2:  1.0000 0.0000
#> 
#> $E
#>            Pr(1)  Pr(2)
#> class 1:  0.0214 0.9786
#> class 2:  0.7771 0.2229
#> 
#> $F
#>            Pr(1)  Pr(2)
#> class 1:  0.5773 0.4227
#> class 2:  1.0000 0.0000
#> 
#> $G
#>            Pr(1)  Pr(2)
#> class 1:  0.0000 1.0000
#> class 2:  0.8835 0.1165
#> 
#> Estimated class population shares 
#>  0.5012 0.4988 
#>  
#> Predicted class memberships (by modal posterior prob.) 
#>  0.5 0.5 
#>  
#> ========================================================= 
#> Fit for 2 latent classes: 
#> ========================================================= 
#> number of observations: 118 
#> number of estimated parameters: 15 
#> residual degrees of freedom: 103 
#> maximum log-likelihood: -317.2568 
#>  
#> AIC(2): 664.5137
#> BIC(2): 706.0739
#> G^2(2): 62.36543 (Likelihood ratio/deviance statistic) 
#> X^2(2): 92.64814 (Chi-square goodness of fit) 
#>  
#> Conditional item response (column) probabilities,
#>  by outcome variable, for each class (row) 
#>  
#> $A
#>            Pr(1)  Pr(2)
#> class 1:  0.0000 1.0000
#> class 2:  0.9427 0.0573
#> class 3:  0.4872 0.5128
#> 
#> $B
#>            Pr(1)  Pr(2)
#> class 1:  0.0191 0.9809
#> class 2:  0.8621 0.1379
#> class 3:  0.0000 1.0000
#> 
#> $C
#>            Pr(1)  Pr(2)
#> class 1:  0.1425 0.8575
#> class 2:  1.0000 0.0000
#> class 3:  1.0000 0.0000
#> 
#> $D
#>            Pr(1)  Pr(2)
#> class 1:  0.4138 0.5862
#> class 2:  1.0000 0.0000
#> class 3:  0.9424 0.0576
#> 
#> $E
#>            Pr(1)  Pr(2)
#> class 1:  0.0000 1.0000
#> class 2:  0.9449 0.0551
#> class 3:  0.2494 0.7506
#> 
#> $F
#>            Pr(1)  Pr(2)
#> class 1:  0.5236 0.4764
#> class 2:  1.0000 0.0000
#> class 3:  1.0000 0.0000
#> 
#> $G
#>            Pr(1)  Pr(2)
#> class 1:  0.0000 1.0000
#> class 2:  1.0000 0.0000
#> class 3:  0.3693 0.6307
#> 
#> Estimated class population shares 
#>  0.4447 0.3736 0.1817 
#>  
#> Predicted class memberships (by modal posterior prob.) 
#>  0.4322 0.3729 0.1949 
#>  
#> ========================================================= 
#> Fit for 3 latent classes: 
#> ========================================================= 
#> number of observations: 118 
#> number of estimated parameters: 23 
#> residual degrees of freedom: 95 
#> maximum log-likelihood: -293.705 
#>  
#> AIC(3): 633.41
#> BIC(3): 697.1357
#> G^2(3): 15.26171 (Likelihood ratio/deviance statistic) 
#> X^2(3): 20.50335 (Chi-square goodness of fit) 
#>  
```
