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
      x[["transition"]], move >= from, move <= to, value > 0L)) %>%
    dplyr::arrange(move)

  # finally, for each (x, y), the first entry (smallest move number)
  # is the ones to be marked in the kifu
  # the other moves are listed outside
  flg <- !duplicated(dplyr::select(out, x, y))

  # define three separate data frames
  # init    ... initial board state (shown on the board, no number)
  # number  ... moves to be shown on the board with number
  # noted   ... moves to be listed outside
  init   <- dplyr::filter(out, move == 0L)
  number <- dplyr::filter(out, move != 0L, flg)
  noted  <- dplyr::filter(out, move != 0L, !flg)
  return(structure(.Data = list(init = init, number = number, noted = noted,
                                boardsize = x[["boardsize"]],
                                from = from, to = to),
                   class = "kifu"))
}


#' Draw kifu
#' @param obj \code{kifu} object
#' @param ... arguments passed to \code{\link{ggoboard}}
#' @return \code{ggplot} object
#' @export
plot.kifu <- function(obj, stonesize = 6,
                      blackcolor = "#000000", whitecolor = "#ffffff",
                      linecolor = "#000000", textcolor = "#000000",
                      ...)
{
  # initialize board
  out <- ggoboard(obj[["boardsize"]], ...)

  # add initial stones


  return(out)
}