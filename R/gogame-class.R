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

  # rename 'value' as 'color'
  out <- dplyr::rename(out, color = value)

  return(out)
}


#' Plot the go board state by ggplot
#' @param x \code{gogame} object
#' @param at Move number (integer)
#' @param marklast If specifified, add a marker to the last stone (logical)
#' @param lastmarker character of marker indicating the last move
#' @param ... graphic parameters
#' @return \code{ggplot} object
#' @export
plotat <- function(x, at,
                   marklast = TRUE, lastmarker = intToUtf8(9650), ...)
{
  stopifnot("gogame" %in% class(x))

  dat <- stateat(x, at)
  out <- ggoban(x[["boardsize"]], ...) %>%
    addstones(dat$x, dat$y, dat$color)
  if (marklast) {
    dat2 <- dplyr::filter(x$transition, move <= at, move >= 1L, value > 0L) %>%
      dplyr::arrange(move) %>% tail(1)
    if (nrow(dat2) == 1L)
      out <- addlabels(out, dat2$x, dat2$y, lastmarker, dat2$value)
  }
  return(out)
}


