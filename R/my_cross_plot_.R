my_cross_plot_ <- function(.data, .x, .y){
  .x <- rlang::enquo(.x)
  .y <- rlang::enquo(.y)
  ggplot2::ggplot(.data,
                  ggplot2::aes(forcats::fct_rev(factor(!!.x)),
                               fill = forcats::fct_rev(factor(!!.y)))
  )+
    ggplot2::geom_bar(position = 'fill', width = 0.6)+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    ggplot2::coord_flip()+
    ggplot2::labs(x = rlang::as_label(.x), y = 'percentage', fill = rlang::as_label(.y))
}
