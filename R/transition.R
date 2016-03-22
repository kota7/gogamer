
#' Obtain the board transition from moves
#' @param moves Moves
#' @return transition.
#' @export
get_transition <- function()
{
  isMoves <- c(FALSE, FALSE, TRUE, TRUE)
  locations <- c(25, 57, 30, 51)
  colors <- c(1, 1, 2, 1)
  boardsize <- 9
  GetTransition(isMoves, locations, colors, boardsize)
}

