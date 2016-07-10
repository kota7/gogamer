### S3 class 'kifu' is defined ###


#' Constructor of kifu (game record) object
#' @param x \code{gogame} object
#' @param from,to integers specifying the range of moves`
#' @return \code{kifu} object
#' @export
kifu <- function(x, from = 1L, to = 100L)
{
  out <- x %>%
    # obtain the board state just before 'from'
    # and define the move number as 0
    # this part is regarded as the 'initial' state of the kifu
    stateat(from - 1L) %>% dplyr::mutate(move = 0L) %>%
    # then append the moves between 'from' to 'to'
    # note that moves are the ones with positive values
    # these are candidate move numbers to be shown in the kifu
    dplyr::bind_rows(dplyr::filter(
      x[["transition"]], move >= from, move <= to, value > 0L) %>%
        dplyr::rename(color = value)) %>%
    dplyr::arrange(move)

  # finally, for each (x, y), the first entry (smallest move number)
  # is the ones to be marked in the kifu
  # the other moves are listed outside
  flg <- !duplicated(dplyr::select(out, x, y))

  # define three separate data frames
  # init      ... initial board state (shown on the board, no number)
  # numbered  ... moves to be shown on the board with number
  # noted     ... moves to be listed outside
  init     <- dplyr::filter(out, move == 0L)
  numbered <- dplyr::filter(out, move != 0L, flg)
  noted    <- dplyr::filter(out, move != 0L, !flg)
  return(structure(
    .Data = list(init = init, numbered = numbered, noted = noted,
                 boardsize = x[["boardsize"]], from = from, to = to),
    class = "kifu"))
}


#' @export
print.kifu <- function(x, ...)
{
  cat("*** kifu ***\n\n")
  cat("under constraction")
}

#' @export
as.list.kifu <- function(x, ...)
{
  return(x[])
}


#' Draw kifu
#' @param x \code{kifu} object
#' @param y not in use (just for argument consistency with generic function)
#' @param vertical logical indicating how board and notes are aligned
#' @param ... graphical paramters
#' @return list of three graphic objects:
#' \code{board}: \code{ggplot2} object of the board image,
#' \code{notes}: \code{ggplot2} object of the outside note, and
#' \code{combined}: \code{gtable} object the combine them.
#' one for the board, the other for outside note
#' @export
plot.kifu <- function(x, y, vertical = TRUE, ...)
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
  out2 <- kifunote(x, ...)

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

