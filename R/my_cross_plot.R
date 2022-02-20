# Cross tabulations for categorical variables
#'
#' Generate a plot converted from a contingency table.
#'
#' @param .data input a tibble or data.frame
#' @param .x first variable which will be row names of the table
#' @param .y second variable which will be columns of the table
#' @param row_percent if `TRUE`, shows row percent with text, default is `TRUE`
#' @param text_color color of percent text
#' @param text_size size of text
#' @param p.value if `TRUE`, calculates p value of chi-squared test. default is `TRUE`
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
my_cross_plot <- function(.data, .x, .y, row_percent = TRUE, text_color = 'black', text_size = 4,
                          p.value = FALSE){
  .x <- rlang::enquo(.x)
  .y <- rlang::enquo(.y)

  .aggtable <-
    dplyr::select(.data, !!.x, !!.y) %>%
    dplyr::group_by(!!.x, !!.y) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(proportion = n/sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent = purrr::map_chr(proportion, ~{stringr::str_interp('$[.1f]{.*100}%')}))

  .plot <-
    ggplot2::ggplot(.aggtable,
                    ggplot2::aes(forcats::fct_rev(factor(!!.x)), proportion,
                                 fill = forcats::fct_rev(factor(!!.y)))
    )+
    ggplot2::geom_bar(position = 'fill', stat = 'identity', width = 0.6)
    if(row_percent == TRUE){
      .plot <-
        .plot+
        ggplot2::geom_text(aes(y = n, label = percent),
                           color = text_color,
                           size = text_size,
                           position = ggplot2::position_fill(vjust = 0.5))
    }
  .plot <-
    .plot+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    ggplot2::coord_flip()+
    ggplot2::labs(x = rlang::as_label(.x), y = 'percentage', fill = rlang::as_label(.y))+
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = 'bottom')+
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  if(p.value == TRUE){
    .p.value <-
      janitor::tabyl(.data, !!.x, !!.y) %>%
      janitor::chisq.test() %>%
      .$p.value

    .plot <-
      .plot+
      ggplot2::labs(caption = stringr::str_interp('chisq.test: p = $[.4f]{.p.value}'))

  }

  return(.plot)
}


