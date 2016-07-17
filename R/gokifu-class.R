

#' Kifu (go game record) for a range of move
#' @param init     initial board state (\code{data.frame})
#' @param numbered moves to be numberd on the board (\code{data.frame})
#' @param noted    moves to be listed ontside the board (\code{data.frame})
#' @param boardsize integer of board size
#' @return \code{gokifu} object
#' @export
gokifu <- function(init, numbered, noted, boardsize)
{
  # obetain the move numbers
  if (nrow(numbered) == 0) {
    from <- NA_integer_
    to   <- NA_integer_
  } else {
    from <- min(numbered$move)
    to   <- max(numbered$move)
  }

  return(
    structure(
      .Data = list(init = init, numbered = numbered, noted = noted,
                   boardsize = boardsize, from = from, to = to),
      class = "gokifu")
  )
}


#' Print kifu on console
#' @param x \code{gokifu} object
#' @param adjust_origin Logical. If this is true, numbers are
#' deducted by a multiple of 100 if appropriate
#' @param ... graphical parameters
#' @export
print.gokifu <- function(x, adjust_origin = TRUE, ...)
{
  graphic_param <- set_graphic_param(...)

  # trancate x and y labels
  graphic_param$xlabels <- graphic_param$xlabels[1:x$boardsize]
  graphic_param$ylabels <- graphic_param$ylabels[1:x$boardsize]

  # origin is the new origin
  if (adjust_origin) {
    origin <- floor(min(x$numbered$move) / 100) * 100
  } else {
    origin <- 0L
  }

  # header
  x$numberd <- dplyr::arrange_(x$numbered, ~move)
  color1 <- ifelse(x$numbered$color[1] == BLACK, "Black", "White")
  color2 <- ifelse(utils::tail(x$numbered$color, 1) == BLACK, "Black", "White")
  move1 <- x$numbered$move[1]
  move2 <- utils::tail(x$numbered$move, 1)
  w <- sprintf("%s %d - %s %d", color1, move1 - origin, color2, move2 - origin)
  if (origin > 0L) {
    w <- paste(w, sprintf("(%d - %d)", move1, move2), sep = " ")
  }
  cat(w, "\n\n")


  # initial board state
  y <- matrix(graphic_param$emptymark, nrow = x$boardsize, ncol = x$boardsize)
  mark <- ifelse(x$init$color == BLACK,
                 graphic_param$blackmark, graphic_param$whitemark)
  y[cbind(x$init$y, x$init$x)] <- mark

  # insert numbers
  y[cbind(x$numbered$y, x$numbered$x)] <- as.character(x$numbered$move - origin)
  y[] <- sprintf("%3s", y)
  y <- apply(y, 1, paste0, collapse = "")


  # add vertical label
  y <- paste(sprintf("%3s| ", graphic_param$ylabels), y, sep = "")

  # flip y-axis so that the  origin is at the left bottom
  y <- rev(y)

  # add horizontal label
  y <- c(paste("     ",
               sprintf("%3s", graphic_param$xlabels) %>% paste0(collapse = ""),
               sep = ""),
         paste("     ", paste0(rep("---", x$boardsize), collapse = ""), sep= ""),
         y)
  y <- paste0(y, collapse = "\n")
  cat(y, "\n")

  # outside note
  z <- sprintf("%s%d=%s%s",
               ifelse(x$noted$color == BLACK, "B", "W"),
               x$noted$move - origin,
               graphic_param$xlabels[x$noted$x],
               graphic_param$ylabels[x$noted$y])
  maxlen <- x$boardsize * 3L  # nchar per line
  cumlen <- cumsum(nchar(z) + 2)
  linenum <- floor(cumlen/maxlen)
  z <- data.frame(z, linenum, stringsAsFactors = FALSE) %>%
    dplyr::group_by_(~linenum) %>%
    dplyr::summarize_(~paste0(z, collapse = "; ")) %>%
    `[[`(2) %>%
    paste0(collapse = "\n")
  cat("\n  ", z, "\n")
}


#' @export
as.list.gokifu <- function(x, ...)
{
  return(x[])
}


#' Draw kifu
#' @param x \code{kifu} object
#' @param y not in use (just for argument consistency with generic function)
#' @param adjust_origin  Logical.  If this is true, numbers are deducted
#' by a multiple of 100 when appropriate
#' @param ... graphical paramters
#' @return \code{\link{ggkifu}} object
#' @export
plot.gokifu <- function(x, y, adjust_origin = TRUE, ...)
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
  #return(list(out1, out2))
  return(ggkifu(board = out1, note = out2,
                boardsize = x$boardsize, notenrow = 3))
  # TODO: notenrow must be calculated
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

