# Convert p.value to stars
#'
#' convert p value to stars in a tibble.
#'
#' @param .data neko
#'
#' @export
#'
p_star <- function(.data, .var){
  .var <- rlang::enquo(.var)
  .var_name <- rlang::as_label(.var)
  dplyr::mutate(.data,
                !!.var_name := dplyr::case_when(!!.var < 0.001 ~ '***',
                                                !!.var < 0.01 ~ ' **',
                                                !!.var < 0.05 ~ '  *',
                                                !!.var < 0.1 ~ '  +',
                                                TRUE ~ '   ')
  )
}
