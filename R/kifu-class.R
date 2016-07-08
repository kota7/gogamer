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
  out1 <- ggoban(x[["boardsize"]], ...) %>%
    # add initial stones
    # it is okay to have data with no rows
    addstones(x$init$x, x$init$y, x$init$color, ...) %>%
    # add numbered stones
    addstones(x$numbered$x, x$numbered$y,
              x$numbered$color, x$numbered$move, ...)

  # outside note

  return(out1)
}


