# Discriptive statistics for categorical variables
#'
#' Show Discriptive statistics for categorical variables. The output is formatted with `gt` so that you can easily include it in Rmarkdown documents.
#'
#' @param .data input a tibble or data.frame
#' @param ... categorical variables
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom purrr map2_dfr
#' @importFrom purrr map_chr
#' @importFrom janitor tabyl
#' @importFrom stringr str_interp
#' @importFrom gt gt
#' @importFrom gt cols_align
#' @importFrom gt tab_style
#' @importFrom gt tab_options
#' @importFrom gt cell_text
#' @importFrom gt cells_body
#' @importFrom gt cells_row_groups
#' @importFrom gt pct
#'
#' @export
#'

describe_d_ <- function(.data, ...){
  .data <- select(.data, ...)
  tab <-
    map2_dfr(.x = .data, .y = names(.data),
             ~{tabyl(.x) %>%
                 as_tibble() %>%
                 transmute(group = .y,
                           variables = as.character(.x),
                           n, percent)
             }) %>%
    mutate(percent = map_chr(percent, ~{str_interp('$[.1f]{.*100}%')}))

  gt_obj <-
    gt(tab, groupname_col = 'group') %>%
    cols_align(align = 'right', columns = c(n, percent)) %>%
    tab_style(style = cell_text(weight = 'bold'),
              locations = cells_row_groups()
    ) %>%
    tab_style(style = cell_text(indent = pct(5)),
              locations = cells_body(columns = variables)
    ) %>%
    tab_options(table.width = pct(50))

  return(gt_obj)
}
