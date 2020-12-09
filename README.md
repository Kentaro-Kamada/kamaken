# kamaken
* useful functions only for the author of this package

This package is packed functions customized for the author's workflow, so it may be unuseful for a lot of users.  
This package could be suddenly modified at the author's convinience.  

# Functions
* Now, this package contains 10 functions.

## `my_cross`
* A function makes a cross table molded with `gt` package.  
* Pipe friendly. You can use NSE(which is adopted in `Tidyverse`).  
* depends on `dplyr` `tidyr` `forcats` `purrr` `rlang` `stringr` `janitor` `vcd` `gt` `kamaken`.
* augment
  * `.data`: input a data.frame  
  * `.x`: first variable which will be row names of the table.
  * `.y`: second variable which will be columns of the table.
  * `cramer`: if `TRUE`, calculates the value of cramer's V. default is `TRUE`.
  * `p.value`: if `TRUE`, calculates p value of chi-squared test. default is `TRUE`.
  * `adjres`: if `TRUE`, calculates adjusted residual and shows the results of chi-square residual tests. default is `FALSE`. 
