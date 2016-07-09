
#' Read and parse a SGF file
#'
#' @name read_sgf
#' @param path    character string of the path to
#'   a smart go format (SGF) file.  Can be local or online.
#' @param keepfirst logical indicating the branch choice rule
#' @param encoding  character string.
#'   If specified, declares the encoding used on a file.
#'
#' @export
read_sgf <- function(path, keepfirst = TRUE, encoding = "") {
  readLines(path, encoding = encoding) %>%
    paste0(collapse = "\n") %>%
    parse_sgf(keepfirst)
}



#' Parse text of the smart go format.
#'
#' @name parse_sgf
#' @param sgf     scalar character of sgf text.
#' @param keepfirst  logical specifying the branch choice rule.
#'   If TRUE, keep the first appearing branch.
#'   Otherwise, keep the last branch.
#'
#' @export
parse_sgf <- function(sgf, keepfirst = TRUE) {
  ### obtain meta information ###
  proplist <- c(PW = "whitename", WR = "whiterank",
                PB = "blackname", BR = "blackrank",
                SZ = "boardsize", KM = "komi", HA = "handicap",
                DT = "date", EV = "event", RO = "round",
                RE = "result", RU = "rule")
  # obtain properties and rename
  props <- get_props(sgf, names(proplist)) %>% setNames(proplist)

  ### parse plays, comments, and times ###
  sgf <- prune_sgf(sgf, keepfirst)
  moves <- get_moves(sgf)

  ### make gogame object ###
  return(gogame(props, moves))
}
