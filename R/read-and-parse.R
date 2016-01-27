
#' Read and parse a SGF file
#'
#' @name readSGF
#' @param path    A character representing the location of a smart go format (SGF) file.
#' @param encoding  A character string: if non-empty declares the encoding used on a file.
#' @export
readSGF <- function(path, encoding = "") {
  readLines(path, encoding = encoding) %>%
    paste0(collapse = "\n") %>%
    parseSGF()
}



#' Parse text in the smart go format.
#'
#' @name parseSGF
#' @param sgf     A character vector of sgf text.
#'
#' @export
parseSGF <- function(sgf) {
  ### pruning ###
  # TODO: branch pruning


  ### obtain meta information ###
  metatag <- c("PW", "PB", "WR", "BR", "SZ", "KM", "HA",
               "DT", "RU", "EV", "RO")
  metainfo <- find_tags(metatag)


  ### parse moves ###
  # TODO: (handicap tags, AB & AW)
  tmp <- stringr::str_match_all(
    sgf, ";([BW])\\[(.*?)\\]")[[1]][, -1]
  moves <- cbind(
    sapply(tmp[, 2], utf8ToInt) %>% t() - 96,
    ifelse (tmp[, 1]=="B", BLACK, WHITE))
  colnames(moves) <- c("x", "y", "col")
  sgf
}
