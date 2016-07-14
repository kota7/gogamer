
#' Read and parse a SGF file
#'
#' @param path    character string of the path to
#'   a smart go format (SGF) file.  Can be local or online
#' @param keepfirst logical indicating the branch choice rule
#' @param ... arguments passed to \code{\link{readLines}}
#'   If specified, declares the encoding used on a file.
#'
#' @export
read_sgf <- function(path, keepfirst = TRUE, ...) {
  readLines(path, ...) %>%
    paste0(collapse = "\n") %>%
    parse_sgf(keepfirst = keepfirst)
}



#' Parse text of the smart go format.
#'
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
                RE = "result", RU = "rule", PC = "place")
  # obtain properties and rename
  props <- get_props(sgf, names(proplist)) %>% stats::setNames(proplist)

  ### parse plays and points ###
  sgf <- prune_sgf(sgf, keepfirst)
  moves <- get_moves(sgf)
  points <- get_points(sgf)

  ### make gogame object ###
  return(gogame(props, moves, points))
}
