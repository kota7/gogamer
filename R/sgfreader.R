
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
  # test
  sgf <- readLines("tests/sample/joseki.sgf") %>% paste0(collapse = "")

  ## obtain meta information
  proplist <- c(PW = "whitename", WR = "whiterank",
                PB = "blackname", BR = "blackrank",
                SZ = "boardsize", KM = "komi", HA = "handicap",
                DT = "date", EV = "event", RO = "round",
                RE = "result", RU = "rule", PC = "place")
  properties <- gogamer:::get_props(sgf, names(proplist)) %>% stats::setNames(proplist)


  ## parse the sgf into node tree
  tree <- gogamer:::make_sgftree(sgf)
  if (length(tree$data) == 0L) {
    # TODO: do somthing
  }

  ## parse plays and points
  ### each component is a data.frame, where 'id' indicate the node
  parsed <- gogamer:::parse_sgfnode(tree$data)

  ## obtain move number of each node
  ### movenumber maps ID -> moven umber
  hasmove <- rep(FALSE, length(tree$data))
  hasmove[dplyr::filter_(parsed$moves, ~ismove) %>% `[[`("id")] <- TRUE
  movenumber <- gogamer:::get_movenumber(hasmove, tree$children)

  ## compress the tree
  compressor <- gogamer:::tree_compressor(tree$children)
  parsed$moves$id2 <- compressor$indexmap[parsed$moves$id]
  parsed$comments$id2 <- compressor$indexmap[parsed$comments$id]
  parsed$points$id2 <- compressor$indexmap[parsed$points$id]

  ## get boardsize, or guess it if needed
  boardsize <- properties$boardsize %>%
    stringr::str_extract("[0-9]+") %>% as.integer()
  if (nrow(parsed$moves) > 0) {
    maxnum <- max(c(parsed$moves$x, parsed$moves$y))
  } else {
    maxnum <- 0
  }
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
  ### convert moves into a list of dataframes
  ### before doing that, sort the moves dataframe so that
  ### setup plays come after actual moves
  ### by doing so the moves within the same SGF node (i.e. id) gets the same
  ### move number in the Gogame class, and thus one can go back to a
  ### certin game point properly
  ### this consideration would be unnecessary if AB and AW tags appear only
  ### at the beginning of the game, though
  parsed$moves <- dplyr::arrange_(parsed$moves, ~id, ~dplyr::desc(ismove))
  n2 <- length(compressor$parent)
  moves <- lapply(seq(n2), function(i) dplyr::filter_(parsed$moves, ~id2 == i))
  transition_tree <- gogamer:::get_transitiontree(moves, compressor$children, boardsize)


  ### make gogame object ###
  #return(gogame(props, moves, points))
  NULL
}
