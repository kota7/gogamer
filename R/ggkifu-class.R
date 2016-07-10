
#' One-page kifu image
#' @param board \code{ggplot} object of board
#' @param note \code{ggplot} object of outside note
#' @param boardsize integer of board size
#' @param notenrow integer of number of rows of note
#' @return \code{ggkifu} object
ggkifu <- function(board, note, boardsize, notenrow)
{
  return(structure(.Date = list(
    board = board, note = note, boardsize = boardsize, notenrow = notenrow),
    class = "ggkifu"))
}


