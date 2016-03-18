
#' Read and parse a SGF file
#'
#' @name read_sgf
#' @param path    A character string of the path to
#'   a smart go format (SGF) file.  Can be local or online.
#' @param encoding  A character string.
#'   If non-empty declares the encoding used on a file.
#'
#' @export
read_sgf <- function(path, keep_first = FALSE, encoding = "") {
  readLines(path, encoding = encoding) %>%
    paste0(collapse = "\n") %>%
    parse_sgf(keep_first)
}



#' Parse text in the smart go format.
#'
#' @name parse_sgf
#' @param sgf     A character string of sgf text.
#' @param keep_first  Logical specifying the branch choice rule.
#'   If TRUE, keep the first appearing branch.
#'   Otherwise, keep the last branch.
#'
#' @export
parse_sgf <- function(sgf, keep_first = FALSE) {
  ### obtain meta information ###
  tags <- c("PW", "PB", "WR", "BR",
            "RE", "SZ", "KM", "HA",
            "DT", "RU", "EV", "RO")
  metainfo <- get_props(sgf, tags)

  ### parse plays, comments, and times ###
  sgf <- prune_sgf(sgf, keep_first)
  moves <- get_moves(sgf)
  list(metainfo, moves)
}
