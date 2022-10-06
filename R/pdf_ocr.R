# pdf OCR function
#'
#' This function needs some outside modules tesseract, poppler and qpdf. These can be installed from homebrew.
#'
#' @export
#'

pdf_ocr <- function(path, lang = 'eng', dpi = 150) {

  dirpath <- dirname(path)
  filename <- str_remove(path, str_c('^', dirpath))

  # pdfのページ数を調べる
  page <- pdftools::pdf_info(path)$pages

  if(!dir.exists(str_c(dirpath, '/png'))) dir.create(str_c(dirpath, '/png'))
  if(!dir.exists(str_c(dirpath, '/pdf'))) dir.create(str_c(dirpath, '/pdf'))

  pngpath <-
    str_c(dirpath, '/png',
          filename %>% str_remove('\\.pdf$'),
          1:page, '.png')

  pdfpath <-
    str_replace(pngpath, 'png/', 'pdf/') %>%
    str_remove('\\.png$')

  # pdf to png
  pdftools::pdf_convert(path, format = 'png', pages = 1:page, filenames = pngpath, dpi = dpi, verbose = T)

  # ocr
  walk2(pngpath, pdfpath,
        ~{system2('tesseract', args = str_c('"', .x, '" "', .y, '" -l ', lang, ' pdf'))}
  )

  # combine pdf
  pdftools::pdf_combine(input = str_c(pdfpath, '.pdf'), output = str_replace(path, '\\.pdf$', '_out\\.pdf'))

}

