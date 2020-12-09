my_cross_plot <- function(.data, .x, .y, .text = TRUE, .text_color = 'black'){
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
    if(.text == TRUE){
      .plot <-
        .plot+
        ggplot2::geom_text(aes(y = n, label = percent), color = .text_color,
                           position = ggplot2::position_fill(vjust = 0.5))
    }
  .plot <-
    .plot+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    ggplot2::coord_flip()+
    ggplot2::labs(x = rlang::as_label(.x), y = 'percentage', fill = rlang::as_label(.y))

  return(.plot)
}

