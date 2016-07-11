

#' Kifu (go game record) for a range of move
#' @param init     initial board state (\code{data.frame})
#' @param numbered moves to be numberd on board (\code{data.frame})
#' @param noted    moves to be listed ontside of boarde (\code{data.frame})
#' @param boardsize integer of board size
#' @param from,to integers specifying the range of moves`
#' @return \code{gokifu} object
#' @export
gokifu <- function(init, numbered, noted, boardsize, from, to)
{
  return(
    structure(
      .Data = list(init = init, numbered = numbered, noted = noted,
                   boardsize = boardsize, from = from, to = to),
      class = "gokifu")
  )
}


#' @export
print.gokifu <- function(x, ...)
{
  cat("* kifu *\n\n")
  cat("move:", x$from, "-", x$to, "\n")
}


#' @export
as.list.gokifu <- function(x, ...)
{
  return(x[])
}


#' Draw kifu
#' @param x \code{kifu} object
#' @param y not in use (just for argument consistency with generic function)
#' @param ... graphical paramters
#' @return list of three graphic objects:
#' \code{board}: \code{ggplot2} object of the board image,
#' \code{notes}: \code{ggplot2} object of the outside note, and
#' \code{combined}: \code{gtable} object the combine them.
#' one for the board, the other for outside note
#' @export
plot.gokifu <- function(x, y, ...)
{
  # board plot
  out1 <- ggoban(x$boardsize, ...) %>%
    # add initial stones
    # it is okay to have data with no rows
    addstones(x$init$x, x$init$y, x$init$color, ...) %>%
    # add numbered stones
    addstones(x$numbered$x, x$numbered$y,
              x$numbered$color, x$numbered$move, ...)

  # outside note
  if (nrow(x$noted) >= 1L) {
    out2 <- kifunote(x, ...)
  } else {
    out2 <- NULL
  }
  return(list(out1, out2))
}


#' Draw outside note of kifu
#' @param x \code{kifu} object
#' @param ... graphic parameter
#' @return \code{ggplot} object
kifunote <- function(x, ...)
{
  graphic_param <- set_graphic_param(...)

  k <- graphic_param$moveperrow
  colsize <- 4

  n <- nrow(x$noted)

  # positions of each move
  xx <- colsize*((0:(n-1)) %% k) + 1
  yy <- -ceiling((1:n)/k)*2

  # create canvas
  out <- ggplot2::ggplot() + #ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(limits = c(1, colsize*k)) +
    ggplot2::scale_y_continuous(limits = c(min(yy) - 1, max(yy) + 1)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(
                     fill = graphic_param$notebackcolor))

  # add stones
  out <- addstones(out, xx, yy, x$noted$color, x$noted$move,
                   stonesize  = graphic_param$notestonesize,
                   numbersize = graphic_param$notenumbersize)
  # add text
  ll <- paste("    ",
              graphic_param$xlabels[x$noted$x],
              graphic_param$ylabels[x$noted$y], sep = "")
  dat <- data.frame(xx, yy, ll)
  out <- out +
    ggplot2::geom_text(data = dat,
                       ggplot2::aes(x = xx, y= yy, label = ll),
                       color = graphic_param$notetextcolor,
                       size = graphic_param$notetextsize,
                       hjust = 0)
  return(out)
}

