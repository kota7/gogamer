guess_boardsize <- function(boardsize, maxcoord)
{
  # guess boardsize, if needed
  #
  # Args:
  #   boardsize : character or numeric. current boardsize
  #   maxcoord  : maximum coordinate
  # Returns:
  #   integer

  if (!is.numeric(boardsize) && !is.character(boardsize))
    stop("boardsize must be character or number")

  # take the first element, just in case a vector is given
  boardsize <- stringr::str_extract(boardsize[1], "[0-9]+") %>% as.integer()
  if (is.na(boardsize)) {
    if (maxcoord > 19L)
      stop("Coordinates exceed 19, but the boardsize is not specified")
    cat("board size is set to 19\n")
    boardsize <- 19L
  } else if (boardsize < maxcoord) {
    warning("Coordinates exceed the specified board size")
    if (maxcoord <= 9L) {
      boardsize <- 9L
    } else if (maxcoord <= 13L) {
      boardsize <- 13L
    } else if (maxcoord <= 19L) {
      boardsize <- 19L
    } else {
      stop("Coordinates exceed 19, but the given boardsize is smaller")
    }
    cat("board size is set to", boardsize, "\n")
  }
  return(boardsize)
}

