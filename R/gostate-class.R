
#' Go game state
#' @description \code{gostate} object stores a go game state including
#' stone configuration on the board and the numbers of prisoners.
#' @param board  a \code{data.frame} representing stone allocation.
#' It must have variables (\code{x}, \code{y}, \code{color})
#' @param boardsize baord size (integer)
#' @param b_captured,w_captured  numbers of captured stone (integer)
#'
#' @return \code{gostate} returns an object of class "gostate"
#'
#' @examples
#' gostate(data.frame(x = 4, y = 4, color = 1), 19, 0, 0)
#' @export
gostate <- function(board, boardsize, b_captured, w_captured)
{
  # argument check
  if (!is.data.frame(board))
    stop("board must inherits data.frame")
  if (!all(c("x", "y", "color") %in% names(board)))
    stop("board must have variables 'x' 'y' and 'color'")

  out <- structure(
    .Data = list(board = board, boardsize = boardsize,
                 b_captured = b_captured, w_captured = w_captured),
    class = "gostate")
  return(out)
}


#' @export
print.gostate <- function(x, ...)
{
  cat("* board state\n")
  print(x$board)
  cat("\n")
  cat("* boardsize =", x$boardsize, "\n\n")
  cat("* captured stones\n")
  cat("  black:", x$b_captured,
      "  white:", x$w_captured, "\n")
}


#' @export
plot.gostate <- function(x, y, ...)
{
  out <- ggoban(x$boardsize, ...) %>%
    addstones(x$board$x, x$board$y, x$board$color)
  return(out)
}
