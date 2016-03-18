
#' Read and parse a SGF file
#'
#' @name readSGF
#' @param path    A character string of the path to
#'   a smart go format (SGF) file.  Can be local or online.
#' @param encoding  A character string.
#'   If non-empty declares the encoding used on a file.
#'
#' @export
readSGF <- function(path, encoding = "") {
  readLines(path, encoding = encoding) %>%
    paste0(collapse = "\n") %>%
    parseSGF()
}



#' Parse text in the smart go format.
#'
#' @name parseSGF
#' @param sgf     A character string of sgf text.
#'
#' @export
parseSGF <- function(sgf) {
  ### pruning ###
  # TODO: branch pruning

  ### obtain meta information ###
  tags <- c("PW", "PB", "WR", "BR",
            "RE", "SZ", "KM", "HA",
            "DT", "RU", "EV", "RO")
  metainfo <- get_props(sgf, tags)

  ### parse plays, comments, and times ###
  moves <- get_moves(sgf)
  sgf
}
