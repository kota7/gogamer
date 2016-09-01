
#' One-page kifu image
#' @param board \code{ggplot} object of board
#' @param note \code{ggplot} object of outside note
#' @param boardsize integer of board size
#' @param heights  Numeric vector of size two, indicates the vertical size
#' ratio between board and note
#' @param savesize Numeric vector of size two, indicating
#' appropriate pair of width and height when saving
#' @return \code{ggkifu} object
ggkifu <- function(board, note, boardsize,
                   heights = NULL, savesize = NULL)
{
  if (is.null(note)) {
    x <- board
  } else {
    x <- gridExtra::arrangeGrob(board, note, heights = heights)
  }

  # keep components separately
  x$board <- board
  x$note <- note

  attr(x, "boardsize") <- boardsize
  attr(x, "savesize") <- savesize
  class(x) <- c("ggkifu", class(x))
  return(x)
}




#' @export
print.ggkifu <- function(x, ...)
{
  # by calling plot instead of print, no message is shown
  # currently plot method is not overloaded, hence what is called is
  # plot method for ggplot or gg

  # if you use print, suggested size is printed
  # if you use plot, no message is shown
  graphics::plot(x, ...)
  wd <- attr(x, "savesize")[1]
  ht <- attr(x, "savesize")[2]
  cat("suggested size for saving:\n",
      sprintf("  width = %.2f, height = %.2f\n", wd, ht))
}


#' Check if the object is ggkifu class
#' @param x R object
#' @return logical
#' @export
is.ggkifu <- function(x)
{
  # Check if object is ggkifu class
  #
  # Returns:
  #   logical
  return(inherits(x, "ggkifu"))
}


#' Suggested size used for saving
#' @param obj \code{ggoban} or \code{ggkifu} object
#' @return Numeric vector of size two
#' @export
suggested_size <- function(obj)
{
  if (!is.ggkifu(obj) && !is.ggoban(obj))
    stop("object must be either ggoban or ggkifu class")
  return(attr(obj, "savesize"))
}

