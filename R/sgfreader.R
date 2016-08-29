
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
  properties <- get_props(sgf, names(proplist)) %>% stats::setNames(proplist)

  # test
  sgf <- readLines("tests/sample/joseki.sgf") %>% paste0(collapse = "")

  ## parse plays and points
  tree <- make_sgftree(sgf)
  parsed <- parse_sgfnode(tree$data)

  ## obtain move number of each node
  hasmove <- lapply(parsed$moves, function(m) any(m[["ismove"]])) %>% unlist()
  movenumber <- get_movenumber(hasmove, tree$children)

  ## give move number to each parsed data
  ### TODO, need to do this before compression


  ## compress the tree
  compressor <- tree_compressor(tree$children)
  moves <- lapply(compressor$indices, function(i) parsed$moves[i]) %>%
    lapply(dplyr::bind_rows)

  ## get boardsize, or guess it if needed
  boardsize <- properties$boardsize %>%
    stringr::str_extract("[0-9]+") %>% as.integer()
  maxnum <- dplyr::bind_rows(moves) %>% `[`(c("x", "y") ) %>%
    unlist() %>% max()
  if (is.na(boardsize)) {
    if (maxnum > 19L)
      stop("Coordinates exceed 19, but the boardsize is not specified")
    cat("board size is set to 19\n")
    boardsize <- 19L
  } else if (boardsize < maxnum) {
    warning("Coordinates exceed the specified board size")
    if (maxnum <= 9L) {
      boardsize <- 9L
    } else if (maxnum <= 13L) {
      boardsize <- 13L
    } else if (maxnum <= 19L) {
      boardsize <- 19L
    } else {
      stop("Coordinates exceed 19, but the given boardsize is smaller")
    }
  }

  ## obtain the transitions



  ### make gogame object ###
  #return(gogame(props, moves, points))
  NULL
}
