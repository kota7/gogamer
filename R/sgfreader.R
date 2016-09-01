
#' Read and parse a SGF file
#'
#' @param path   character string of the path to
#' a smart go format (SGF) file. Can be local or online
#' @param ... arguments passed to \code{\link{readLines}}
#' @return \code{\link{gogame}} object
#' @export
read_sgf <- function(path, ...) {
  readLines(path, ...) %>%
    paste0(collapse = " ") %>%
    parse_sgf() %>%
    return()
}



#' Parse text of the smart go format.
#'
#' @param sgf scalar character of sgf text.
#' @return \code{\link{gogame}} object
#' @export
parse_sgf <- function(sgf) {
  ## obtain meta information
  proplist <- c(PW = "whitename", WR = "whiterank",
                PB = "blackname", BR = "blackrank",
                SZ = "boardsize", KM = "komi", HA = "handicap",
                DT = "date", EV = "event", RO = "round",
                RE = "result", RU = "rule", PC = "place")
  properties <- get_props(sgf, names(proplist)) %>% stats::setNames(proplist)


  ## parse the sgf into node tree
  ### create tree of nodes
  tree <- make_sgftree(sgf)

  ## edge case where SGF has no node
  if (length(tree$data) == 0L) {
    stop("Invalid SGF (No node has been found)")
  }

  ## parse plays and points
  ### each component is a data.frame, where 'id' indicate the node
  parsed <- parse_sgfnode(tree$data)

  ## obtain move number of each node
  ### movenumber maps ID -> moven umber
  hasmove <- rep(FALSE, length(tree$data))
  hasmove[dplyr::filter_(parsed$moves, ~ismove) %>% `[[`("id")] <- TRUE
  movenumber <- get_movenumber(hasmove, tree$children)

  ### add move numbers
  parsed$moves$move    <- movenumber[parsed$moves$id]
  parsed$points$move   <- movenumber[parsed$points$id]
  parsed$comments$move <- movenumber[parsed$comments$id]

  ## compress the tree
  compressor <- tree_compressor(tree$children)
  parsed$moves$nodeid    <- compressor$indexmap[parsed$moves$id]
  parsed$comments$nodeid <- compressor$indexmap[parsed$comments$id]
  parsed$points$nodeid   <- compressor$indexmap[parsed$points$id]

  ## get boardsize, or guess it if needed
  maxcoord <- max(c(0L, parsed$moves$x, parsed$moves$y))
  boardsize <- guess_boardsize(properties$boardsize, maxcoord)
  properties$boardsize <- boardsize

  ## revert the y-coordinate
  ### In SGF, top-left is the origin
  ### convert so the bottom-left is the origin since
  ### note: coordinate zero must be kept since they represent pass
  flg <- (parsed$moves$y > 0L)
  parsed$moves$y[flg] <- boardsize + 1L - parsed$moves$y[flg]
  flg <- (parsed$points$y > 0L)
  parsed$points$y[flg] <- boardsize + 1L - parsed$points$y[flg]
  rm(flg)

  ## sort the moves dataframe so that setup plays come after actual moves
  ### by doing so the moves within the same SGF node (i.e. id) gets the same
  ### move number in the Gogame class, and thus one can go back to a
  ### certin game point properly
  ### this consideration would be unnecessary if AB and AW tags appear only
  ### at the beginning of the game, though
  parsed$moves <- dplyr::arrange_(parsed$moves, ~id, ~dplyr::desc(ismove))

  ## obtain the transitions
  transitions <- get_transition_wrapper(
    parsed$moves, boardsize, compressor$children)

  ## A check for move numebr consistency
  ### do moves and transtion have the same set of move numbers within nodes?
  check <- identical(
    dplyr::group_by_(parsed$moves, ~nodeid) %>%
      dplyr::summarize_(~min(move), ~max(move)),
    dplyr::group_by_(transitions, ~nodeid) %>%
      dplyr::summarize_(~min(move), ~max(move))
  )
  if (!check) {
    warning("move numbers may not be correct for comments and points")
  }

  ## clean up
  ### id is removed
  parsed$moves    <- parsed$moves %>% dplyr::select_(~ -id)
  parsed$comments <- parsed$comment %>% dplyr::select_(~ -id)
  parsed$points   <- parsed$points %>% dplyr::select_(~ -id)

  ### make gogame object ###
  return(gogame(
    properties = properties, gametree = list(
      transition = transitions, move = parsed$moves ,
      point = parsed$points, comment = parsed$comments,
      parent = compressor$parent,
      children = compressor$children, leaf = compressor$leaf)
  ))
}
