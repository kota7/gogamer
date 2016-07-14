
#' One-page kifu image
#' @param board \code{ggplot} object of board
#' @param note \code{ggplot} object of outside note
#' @param boardsize integer of board size
#' @param notenrow integer of number of rows of note
#' @return \code{ggkifu} object
ggkifu <- function(board, note, boardsize, notenrow)
{
  if (is.null(note)) {
    x <- board
  } else {
    x <- gridExtra::grid.arrange(board, note, heights = c(5, 1))
    # TODO: this height should be altered
  }

  attr(x, "boardsize") <- boardsize
  attr(x, "notenrow") <- notenrow
  class(x) <- c("ggkifu", class(x))
  return(x)
}


#' @export
print.ggkifu <- function(x, ...)
{
  NextMethod()  # print as ggplot or gtable
}