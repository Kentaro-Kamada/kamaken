# kamaken
* useful functions only for the author of this package

This package contains functions customized for the author's workflow, so it may be unuseful for a lot of users.  
This package could be suddenly modified at the author's convinience.  

# Functions
* Now, this package contains 10 functions.

## `my_cross`
* A function makes a cross table molded with `gt` package.  
* Pipe friendly. You can use NSE(which is adopted in `Tidyverse`).  
* Depends on `dplyr` `tidyr` `forcats` `purrr` `rlang` `stringr` `janitor` `vcd` `gt` `kamaken`.
* Augment
  * `.data`: input a data.frame  
  * `.x`: first variable which will be row names of the table.
  * `.y`: second variable which will be columns of the table.
  * `cramer`: if `TRUE`, calculates the value of cramer's V. default is `TRUE`.
  * `p.value`: if `TRUE`, calculates p value of chi-squared test. default is `TRUE`.
  * `adjres`: if `TRUE`, calculates adjusted residual and shows the results of chi-square residual tests. default is `FALSE`. 

* example
```r
# data from ggplot2
diamonds %>% 
  my_cross(cut, clarity, cramer = TRUE, p.value = TRUE)
```
![my_cross_example](https://github.com/Sickle-Sword/files/blob/main/my_cross_example.png)

## `my_cross_plot`
* A function visualizes a cross table.
* Able to use the same syntax as `my_cross`. 
* Depends on `dplyr` `forcats` `purrr` `rlang` `stringr` `janitor` `ggplot2` `scales`
* Augment
  * `.data`: input a data.frame
  * `.x`: first variable
  * `.y`: second variable
  
 
 * example
 ```r
 # data from ggplot2
 diamonds %>% 
   my_cross_plot(cut, clarity, .text = TRUE, .text_color = 'white', .text_size = 4, .pvalue = TRUE)+
   theme_classic()
 ```
 ![my_cross_plot_example](https://github.com/Sickle-Sword/files/blob/main/my_cross_plot_example.png)
