# use LEM (Vermunt 1997) thourgh R
#'
#' Activate LEM through `system2()`.
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


  if(.Platform$OS.type == 'unix' && Sys.getenv('WINEPATH') == '') {
    stop('please set the directory of lem95.exe to the environment variable \'WINEPATH\'')
  }

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
                          file = str_c(path, '/inp/', str_remove(., ' '), '.inp'))})


  # inputファイルとoutputファイルのパス指定
  inp_path <- list.files(str_c(path, '/inp'), full.names = T)
  out_path <- str_replace_all(inp_path, 'inp', 'out')

  # OS判別
  if(.Platform$OS.type == 'windows') {
    command <- 'lem95'
    args <- str_c(inp_path, out_path, sep = ' ')
    env <- character()
  }
  if(.Platform$OS.type == 'unix') {
    command <- 'wine'
    args <- str_c('lem95', inp_path, out_path, sep = ' ')
    env <- str_c('WINEPATH=', Sys.getenv('WINEPATH'))
  }

  # lem実行
  walk(args,
       ~{system2(command, args = ., env = env,
                 stdout = F, stderr = F, wait = F)
         Sys.sleep(0.8)}
  )

}
