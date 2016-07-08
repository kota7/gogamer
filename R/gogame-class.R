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
  flg <- capt[["value"]] == -BLACK
  if (any(flg)) b_captured <- capt[["captured"]][flg]

  w_captured <- 0L
  flg <- capt[["value"]] == -WHITE
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
#' @param linecolor color for stone outline
#' @param ... arguments passed to \code{\link{ggoban}}
#' @return \code{ggplot} object
#' @export
plotat <- function(x, at, stonesize = 6,
                   marklastmove = "",
                   blackcolor = "#000000",
                   whitecolor = "#ffffff",
                   linecolor = "#000000",
                   ...)
{
  stopifnot("gogame" %in% class(x))

  dat <- stateat(x, at)
  out <- ggoban(x[["boardsize"]], ...) +
    ggplot2::geom_point(
      data = dat, ggplot2::aes(x, y), size = stonesize, color = linecolor) +
    ggplot2::geom_point(
      data = dat, ggplot2::aes(x, y, color = factor(value)),
      size = stonesize*0.8) +
    ggplot2::scale_color_manual(guide = FALSE,
                                values = c(blackcolor, whitecolor))

  return(out)
}


