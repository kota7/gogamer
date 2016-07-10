
#' Constructor of kifuplot object
#' @param board \code{ggplot} object of board
#' @param note \code{ggplot} object of outside note
#' @param boardsize integer of board size
#' @param notenrow integer of number of rows of note
#' @return \code{kifuimage} object
kifuimage <- function(board, note, boardsize, notenrow)
{
  return(structure(.Date = list(
    board = board, note = note, boardsize = boardsize, notenrow = notenrow),
    class = "kifuimage"))
}


