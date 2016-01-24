
#' Read and parse a SGF file
#'
#' @name readSGF
#' @param path    A character representing the location of a smart go format (SGF) file.
#' @param fileEncoding  A character string: if non-empty declares the encoding used on a file.
#' @export
readSGF <- function(path, fileEncoding = "") {
  scan(path, what = "character", sep = "\n", fileEncoding = fileEncoding) %>%
    paste(collapse = "") %>%
    parseSGF()
}



#' Parse SGF file(s)
#'
#' @name parseSGF
#' @param sgf     A character vector of sgf text.
#' @export
parseSGF <- function(sgf) {
}
