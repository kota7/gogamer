### S3 class 'gogame' is defined ###


# TODO:
#   should i define functions as generic method?

#' @export
print.gogame <- function(x, ...)
{
  cat("\n*** Go game ***\n")
  cat(sprintf(" %s (W) vs %s\n", x[["PW"]], x[["PB"]]))
  cat(sprintf(" %s (%d moves)\n", x[["RE"]], nrow(x[["transition"]])))
  cat("***\n")
}



#' Return the board state
#'
#' @param x \code{gogame} object
#' @param at integer of the move number
#' @return data frame object
#' @export
stateat <- function(x, at)
{
  stopifnot("gogame" %in% class(x))

  # the following data frame represent the board state in
  # dense matrix format
  out <- x[["transition"]] %>%
    dplyr::filter(move <= at) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::filter(value > 0L)

  # compute the number of prisoners
  capt <- x[["transition"]] %>%
    dplyr::filter(move <= at, value < 0L) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(captured = length(move))

  b_captured <- 0L
  flg <- capt[["value"]] == -1L
  if (any(flg)) b_captured <- capt[["captured"]][flg]

  w_captured <- 0L
  flg <- capt[["value"]] == -2L
  if (any(flg)) w_captured <- capt[["captured"]][flg]

  # add prisoners as attributes of the data frame
  attr(out, "b_captured") <- b_captured
  attr(out, "w_captured") <- w_captured

  return(out)
}


#' Plot the go board state by ggplot
#' @param x \code{gogame} object
#' @param at integer of the move number
#' @param stonesize numeric that indicates the size of stones,
#' passed to \code{geom_point}
#' @param blackcolor color for black stone
#' @param whitecolor color for white stone
#' @param edgecolor color for stone edge
#' @return \code{ggplot} object
#' @export
plotat <- function(x, at, stonesize = 6,
                   blackcolor = "#000000", whitecolor = "#ffffff",
                   edgecolor = "#000000")
{
  stopifnot("gogame" %in% class(x))

  dat <- stateat(x, at)
  out <- ggoboard(x[["boardsize"]]) +
    ggplot2::geom_point(
      data = dat, ggplot2::aes(x, y), size = stonesize, color = edgecolor) +
    ggplot2::geom_point(
      data = dat, ggplot2::aes(x, y, color = value), size = stonesize*0.8) +
    ggplot2::scale_color_continuous(guide = FALSE,
                                    low = blackcolor, high = whitecolor)
  out
}


#' ggplot go board
#' @param boardsize integer of boardsize
#' @param gridcolor color for the grid
#' @param boardcolor color for the background
#' @return \code{ggplot} object of goban
#' @examples
#'   ggoboard(19)
#' @export
ggoboard <- function(boardsize, gridcolor = "#262626", boardcolor = "#e1f0c0")
{
  # dummy data for board grid
  dat <- rbind(
    data.frame(x = 1L, y = seq(boardsize),
               xend = boardsize, yend = seq(boardsize)),
    data.frame(x = seq(boardsize), y = 1L,
               xend = seq(boardsize), yend = boardsize)
  )

  out <- ggplot2::ggplot(dat) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(
      breaks = seq(1, boardsize), limits = c(0.5, boardsize + 0.5), name = "") +
    ggplot2::scale_y_continuous(
      breaks = seq(boardsize, 1), limits = c(boardsize + 0.5, 0.5),
      name = "", trans = "reverse") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = boardcolor)
    ) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color = gridcolor)

  return(out)
}


