describe_d_ <- function(.data, ...){
  .data <- dplyr::select(.data, ...)
  tab <-
    purrr::map2_dfr(.x = .data, .y = names(.data),
                    ~{janitor::tabyl(.x) %>%
                        dplyr::as_tibble() %>%
                        dplyr::transmute(group = .y,
                                         variables = as.character(.x),
                                         n, percent)
                    }) %>%
    dplyr::mutate(percent = purrr::map_chr(percent, ~{stringr::str_interp('$[.1f]{.*100}%')}))

  gt_obj <-
    gt::gt(tab, groupname_col = 'group') %>%
    gt::cols_align(align = 'right', columns = c(n, percent)) %>%
    gt::tab_style(style = gt::cell_text(weight = 'bold'),
                  locations = gt::cells_row_groups()
                  ) %>%
    gt::tab_style(style = gt::cell_text(indent = gt::pct(5)),
                  locations = gt::cells_body(columns = variables)
                  ) %>%
    gt::tab_options(table.width = gt::pct(50))

  return(gt_obj)
}
