# Cross tabulations for categorical variables
#'
#' Generate a plot converted from a contingency table.
#'
#' @param .data input a tibble or data.frame
#' @param .x first variable which will be row names of the table
#' @param .y second variable which will be columns of the table
#' @param row_percent if `TRUE`, shows row percent with text, default is `TRUE`
#' @param text_size size of text
#' @param p.value if `TRUE`, calculates p value of chi-squared test. default is `TRUE`
#'
#' @importFrom magrittr %>%
#' @importFrom rlang enquo
#' @importFrom rlang as_label
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom purrr map_chr
#' @importFrom stringr str_interp
#' @importFrom forcats fct_rev
#' @importFrom janitor tabyl
#' @importFrom janitor chisq.test
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 position_fill
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom scales label_percent
#'
#'
#' @export
#'

my_cross_plot <- function(.data, .x, .y, strata = NULL,
                          row_percent = TRUE, text_size = 4, p.value = FALSE) {
  .x <- enquo(.x)
  .y <- enquo(.y)
  strata <- enquo(strata)

  .aggtable <-
    select(.data, !!strata, !!.x, !!.y) %>%
    group_by(!!strata, !!.x, !!.y) %>%
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(proportion = n/sum(n)) %>%
    ungroup() %>%
    mutate(percent = map_chr(proportion, ~{str_interp('$[.1f]{.*100}%')}))

  .plot <-
    ggplot(.aggtable,
           aes(fct_rev(factor(!!.x)), proportion,
               fill = fct_rev(factor(!!.y)))
    )+
    geom_bar(position = 'fill', stat = 'identity', width = 0.6, color = 'black')

  if(row_percent == TRUE) {
    .plot <-
      .plot+
      geom_label(aes(y = n, label = percent),
                 color = 'black',
                 fill = 'white',
                 size = text_size,
                 position = position_fill(vjust = 0.5))
  }
  .plot <-
    .plot+
    scale_y_continuous(labels = scales::label_percent())+
    coord_flip()+
    facet_grid(rows = vars(!!strata))+
    labs(x = as_label(.x), y = 'percentage', fill = as_label(.y))+
    theme(legend.position = 'bottom')+
    guides(fill = guide_legend(reverse = TRUE))

  if(p.value == TRUE) {
    .p.value <-
      tabyl(.data, !!.x, !!.y) %>%
      chisq.test() %>%
      .$p.value

    .plot <-
      .plot+
      labs(caption = str_interp('Pearson\'s Chi-squared test: p = $[.4f]{.p.value}'))

  }

  return(.plot)
}

