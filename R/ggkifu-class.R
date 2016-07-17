
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
                   heights = c(7, 1), savesize = c(5, 6))
{
  if (is.null(note)) {
    x <- board
  } else {
    x <- gridExtra::grid.arrange(board, note, heights = heights)
    # TODO: this height should be altered
  }

  attr(x, "boardsize") <- boardsize
  attr(x, "board") <- board
  attr(x, "note") <- note
  attr(x, "savesize") <- savesize
  class(x) <- c("ggkifu", class(x))
  return(x)
}


#' @export
print.ggkifu <- function(x, quiet = FALSE, ...)
{
  NextMethod()  # print as ggplot or gtable
  if (!quiet) {
    wd <- attr(x, "savesize")[1]
    ht <- attr(x, "savesize")[2]
    cat("\nsuggested size for saving:\n",
        sprintf("  width = %.2f, height = %.2f\n", wd, ht))
  }
}


#' @param x An R object
#' @return Logical.
#' @export
#' @rdname ggkifu
is.ggkifu <- function(x)
{
  return(inherits(x, "ggkifu"))
}


#' Suggested size used for saving
#' @param obj \code{ggkifu} object
#' @return Numeric vector of size two
#' @export
suggested_size <- function(obj)
{
  if (!is.ggkifu(obj)) stop("object is not ggkifu class")
  return(attr(obj, "savesize"))
}

