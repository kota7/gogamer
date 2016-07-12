
#' Go game state
#' @description \code{gostate} object stores a go game state including
#' stone configuration on the board and the numbers of prisoners.
#' @param board  a \code{data.frame} representing stone allocation.
#' It must have variables (\code{x}, \code{y}, \code{color})
#' @param boardsize baord size (integer)
#' @param b_captured,w_captured  numbers of captured stone (integer)
#' @param lastmove integer vector of length three that indicates the last move
#' location and color in the order of (x, y, color)
#' @return \code{gostate} returns an object of class "gostate"
#'
#' @examples
#' gostate(data.frame(x = 4, y = 4, color = 1), 19, 0, 0)
#' @export
gostate <- function(board, boardsize, b_captured, w_captured, lastmove = NULL)
{
  # argument check
  if (!is.data.frame(board))
    stop("board must inherits data.frame")
  if (!all(c("x", "y", "color") %in% names(board)))
    stop("board must have variables 'x' 'y' and 'color'")

  out <- structure(
    .Data = list(board = board, boardsize = boardsize,
                 b_captured = b_captured, w_captured = w_captured,
                 lastmove = lastmove),
    class = "gostate")
  return(out)
}


#' Draw board state on console
#'
#' @param x \code{gostate} object
#' @param ... graphic parameters
#'
#' @export
#' @method print gostate
#'
#' @examples
#' stateat(mimiaka, 127)
print.gostate <- function(x, ...)
{
  ### print board state on console

  graphic_param <- set_graphic_param(...)

  # trancate x and y labels
  graphic_param$xlabels <- graphic_param$xlabels[1:x$boardsize]
  graphic_param$ylabels <- graphic_param$ylabels[1:x$boardsize]

  y <- matrix(graphic_param$emptymark, nrow = x$boardsize, ncol = x$boardsize)
  mark <- ifelse(x$board$color == BLACK,
                 graphic_param$blackmark, graphic_param$whitemark)
  y[cbind(x$board$y, x$board$x)] <- mark
  y[] <- sprintf("%2s", y)
  y <- apply(y, 1, paste0, collapse = "")

  # add vertical label
  y <- paste(sprintf("%2s| ", graphic_param$ylabels), y, sep = "")

  # flip y-axis so that the  origin is at the left bottom
  y <- rev(y)

  # add horizontal label
  y <- c(paste("    ",
               sprintf("%2s", graphic_param$xlabels) %>% paste0(collapse = ""),
               sep = ""),
         paste("    ", paste0(rep("--", x$boardsize), collapse = ""), sep= ""),
         y)
  y <- paste0(y, collapse = "\n")
  cat(y)

  cat("\n\n")
  cat("  black captured:", x$b_captured,
      "  white captured:", x$w_captured, "\n")
  if (!is.null(x$lastmove)) {
    color <- ifelse(x$lastmove[3] == BLACK, "black", "white")
    xpos <- graphic_param$xlabels[x$lastmove[1]]
    ypos <- graphic_param$ylabels[x$lastmove[2]]
    cat(sprintf("  last move: %s %s%s\n", color, xpos, ypos))
  }
  NULL
}


#' Draw go board state as graphic
#'
#' @param x \code{gostate} object
#' @param y not in use
#' @param marklast logical indicating if last move should be marked
#' @param ... graphic parameters
#'
#' @return \code{ggoban} object, which inherits \code{ggplot} class
#'
#' @export
#' @method plot gostate
#' @examples
#' stateat(saikoyo, 116) %>% plot()
plot.gostate <- function(x, y, marklast = TRUE, ...)
{
  # draw stone allocation
  out <- ggoban(x$boardsize, ...) %>%
    addstones(x$board$x, x$board$y, x$board$color, ...)

  # add marker to the last move
  if (marklast && !is.null(x$lastmove)) {
    graphic_param <- set_graphic_param(...)
    out <-  out %>%
      addlabels(x$lastmove[1], x$lastmove[2],
                graphic_param$lastmovemarker, x$lastmove[3], ...)
  }

  return(out)
}
