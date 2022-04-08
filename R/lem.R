# use LEM (Vermunt 1997) thourgh R
#'
#' Activate LEM through `system()`. you need to use Windows and set the system PATH.
#'
#' @importFrom purrr walk
#' @importFrom purrr map_chr
#' @importFrom readr write_lines
#' @importFrom readr read_file
#' @importFrom readr parse_double
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr desc
#' @importFrom dplyr dense_rank
#' @importFrom dplyr arrange
#'
#'
#' @export
#'


lem <- function(lat = '',
                man,
                con = '',
                dim,
                lab = '',
                mod,
                rec = '',
                des = '',
                sta = '',
                dum = '',
                dat,
                ite = '',
                see, path) {
  # inpとoutのディレクトリ作成
  if(!dir.exists(path)) {
    dir.create(path)
    dir.create(str_c(path, '/inp'))
    dir.create(str_c(path, '/out'))
  }

  # ファイルの削除
  file.remove(list.files(path, full.names = T, recursive = T))

  # スクリプト書き出し
  walk(see, ~{write_lines(c(lat, man, con, dim, lab, mod, rec, des, sta, dum, dat, ite, .),
                          file = str_c(path, 'inp/', str_remove(., ' '), '.inp'))})


  # inputファイルとoutputファイルのパス指定
  inp_path <- list.files(str_c(path, 'inp'), full.names = T)
  out_path <- str_replace_all(inp_path, 'inp', 'out')

  # lem実行
  walk(str_c('lem95', inp_path, out_path, sep = ' '),
       ~{system(., wait = F)
         Sys.sleep(0.4)})

  # 最大対数尤度の読み出し
  max_loglik <-
    tibble(
      filepath = out_path,
      loglik = map_chr(filepath, read_file)
    ) %>%
    mutate(
      seed = str_extract(loglik, '(?<=Seed random values   = ).+') %>%
        parse_double(),
      loglik = str_extract(loglik, '(?<=Log-likelihood       = ).+') %>%
        parse_double()
    ) %>%
    arrange(desc(loglik)) %>%
    mutate(rank = dense_rank(desc(loglik)))

  return(max_loglik)
}
