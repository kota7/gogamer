
#' Read and parse a SGF file
#'
#' @param path   character string of the path to
#' a smart go format (SGF) file. Can be local or online
#' @param keepfirst  logical specifying the branch choice rule.
#'   If TRUE, keep the first appearing branch.
#'   Otherwise, keep the last branch.
#' @param ... arguments passed to \code{\link{readLines}}
#' @return \code{\link{gogame}} object
#' @export
read_sgf <- function(path, keepfirst = TRUE, ...) {
  readLines(path, ...) %>%
    paste0(collapse = "\n") %>%
    parse_sgf(keepfirst = keepfirst) %>%
    return()
}



#' Parse text of the smart go format.
#'
#' @param sgf scalar character of sgf text.
#' @param keepfirst  logical specifying the branch choice rule.
#'   If TRUE, keep the first appearing branch.
#'   Otherwise, keep the last branch.
#' @return \code{\link{gogame}} object
#' @export
parse_sgf <- function(sgf, keepfirst = TRUE) {
  ## obtain meta information
  proplist <- c(PW = "whitename", WR = "whiterank",
                PB = "blackname", BR = "blackrank",
                SZ = "boardsize", KM = "komi", HA = "handicap",
                DT = "date", EV = "event", RO = "round",
                RE = "result", RU = "rule", PC = "place")
  props <- get_props(sgf, names(proplist)) %>% stats::setNames(proplist)

  # test
  sgf <- "(;GM[1];AB[pd][dp][pp];W[dc];B[cf];W[cd];B[dj]TB[aa]TW[cc:dd])"

  ## parse plays and points
  tree <- make_sgftree(sgf)
  parsed <- parse_sgfnode(tree$data)


  ### make gogame object ###
  #return(gogame(props, moves, points))
  NULL
}
