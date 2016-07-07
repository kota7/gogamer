### S3 class 'kifu' is defined ###


#' Generate kifu (game record) object
#' @param x \code{gogame} object
#' @param from,to integers specifying the range of moves`
#' @return \code{kifu} object
#' @export
kifu <- function(x, from = 1L, to = 100L)
{
  # obtain the board state just before 'from'
  # and define the move number as 0
  # this part is regarded as the 'initial' state of the kifu
  #
  # then append the moves between 'from' to 'to'
  # note that moves are the ones with positive values
  # these are candidate move numbers to be shown in the kifu
  #
  # finally, for each (x, y), the first entry (smallest move number)
  # is the ones to be marked in the kifu
  # the others are to be listed outside

  out <- stateat(x, from - 1L) %>% dplyr::mutate(move = 0L) %>%
    dplyr::bind_rows(dplyr::filter(
      x[["transition"]], move >= from, move <= to, value > 0L)) %>%
    dplyr::arrange(move)
  flg <- !duplicated(dplyr::select(out, x, y))

  # define three separate data frames
  # init    ... initial board state
  # board ... moves to be numbered on the board
  # noted   ... moves to be listed outside
  init  <- dplyr::filter(out, move == 0L)
  board <- dplyr::filter(out, move != 0L, flg)
  noted <- dplyr::filter(out, move != 0L, !flg)
  return(structure(.Data = list(init = init, board = board, noted = noted,
                                boardsize = x[["boardsize"]],
                                from = from, to = to),
                   class = "kifu"))
}


#' Draw kifu
#' @param obj \code{kifu} object
#' @return \code{ggplot} object
#' @export
kifu.plot <- function(obj, stonesize = 6,
                      blackcolor = "#000000", whitecolor = "#ffffff",
                      edgecolor = "#000000", textcolor = "#000000")
{
  # initialize board
  out <- ggoboard(obj[["boardsize"]])

  # add initial stones
}