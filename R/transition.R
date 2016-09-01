get_transition_wrapper <- function(moves, boardsize, children = NULL)
{
  # compute board transition dataframe from moves
  #
  # this function uses either get_transitions or get_transitiontree
  # depending on the input
  #
  # Args:
  #   moves     : data.frame, must contain following variables
  #               x, y, color: integer
  #               ismove     : logical
  #               nodeid     : integer, needed if children is specified
  #   boardsize : integer
  #   children  : list of integer vector, as many as nodeid
  #
  # Returns:
  #   data.frame containing following variables
  #     x, y, value, move, nodeid: integer
  #

  ## input type
  if (!is.data.frame(moves)) stop("moves must be data.frame")
  if (!is.integer(boardsize)) stop("boardsize must be an integer")
  boardsize <- boardsize[1]  # just in case boardsize is a vector
  if (!is.null(children)) {
    if (!is.list(children)) stop("children must be a list of integers")
    if (!all(lapply(children, is.integer) %>% unlist()))
      stop("children must be a list of integers")
  }

  ## if moves contain nothing, return empty transition
  if (nrow(moves) == 0L) {
    return(
      data.frame(x = integer(0), y = integer(0), value = integer(0),
                 move = integer(0), nodeid = integer(0)))
  }

  ## check if necessary variables are contained
  varnames <- c("x", "y", "color", "ismove")
  if (!is.null(children)) varnames <- c(varnames, "nodeid")
  if (!all(varnames %in% names(moves)))
    stop("moves must contain ", paste0(varnames, collapse = ", "))

  ## boardsize must be at least as large as x and y
  if (boardsize < max(c(moves$x, moves$y)))
    stop("coordinates exceed boardsize")

  ## children must be given if there are multiple nodeid
  ## and length of children must be as large as the maximum nodeid
  if ("nodeid" %in% names(moves)) {
    if (max(moves$nodeid) > 1L && is.null(children))
      stop("children must be specified if there are multile nodes")
    if (max(moves$nodeid) > length(children))
      stop("length(children) must be as large as max(nodeid)")
  }

  ## if children is NULL, assume there is only one node
  ## also, if children has only one entry, there is only one node
  if (is.null(children) || length(children) == 1L) {
    out <- get_transitions(boardsize, moves$ismove,
                           moves$x, moves$y, moves$color)
    out$nodeid <- 1L
    return(out)
  }

  ## if children has more than one entries, then there are multiple nodes
  ### convert moves into a list of dataframes before using the tree parser
  out <- get_transitiontree(
    boardsize, moves$ismove, moves$x, moves$y, moves$color, moves$nodeid,
    children, TRUE)
  return(out)
}

