

#' Kifu (go game record) for a range of moves
#' @param unnumbered stones to be shown with no number on the board.
#' These typically are stones put before the move range of the kifu.
#' It also includes setup stones in the range (\code{data.frame})
#' @param numbered moves to be numberd on the board (\code{data.frame})
#' @param noted    moves to be listed ontside the board (\code{data.frame})
#' @param comment  comments during the move range of the game (\code{data.frame})
#' @param boardsize board size (\code{integer})
#' @return \code{gokifu} object
#' @export
gokifu <- function(unnumbered, numbered, noted, comment, boardsize)
{
  # obetain the move numbers
  if (nrow(numbered) == 0L) {
    from <- 0L
    to   <- 0L
  } else {
    from <- min(numbered$move)
    to   <- max(numbered$move)
  }

  return(
    structure(
      .Data = list(unnumbered = unnumbered, numbered = numbered, noted = noted,
                   boardsize = boardsize, from = from, to = to,
                   comment = comment),
      class = "gokifu")
  )
}


#' @export
print.gokifu <- function(x, ...)
{
  graphic_param <- set_graphic_param(...)

  # trancate x and y labels
  graphic_param$xlabels <- graphic_param$xlabels[1:x$boardsize]
  graphic_param$ylabels <- graphic_param$ylabels[1:x$boardsize]

  # set the new origin
  if (graphic_param$adjustorigin && !is.na(x$from)) {
    origin <- floor(x$from / 100) * 100
  } else {
    origin <- 0L
  }

  # header
  if (x$from > 0L) {
    id <- which(x$numbered$move == x$from)
    color1 <- ifelse(x$numbered$color[id] == BLACK, "Black", "White")
    move1  <- x$numbered$move[id]
  } else {
    color1 <- ""
    move1  <- -1
  }
  if (x$to > 0L) {
    id <- which(x$numbered$move == x$to)
    color2 <- ifelse(x$numbered$color[id] == BLACK, "Black", "White")
    move2  <- x$numbered$move[id]
  } else {
    color2 <- ""
    move2  <- -1
  }

  w <- sprintf("%s %d - %s %d", color1, move1 - origin, color2, move2 - origin)
  if (origin > 0L) {
    w <- paste(w, sprintf("(%d - %d)", move1, move2), sep = " ")
  }
  cat(w, "\n\n")


  # unnumbered stones on the board state
  y <- matrix(graphic_param$emptymark, nrow = x$boardsize, ncol = x$boardsize)
  mark <- ifelse(x$unnumbered$color == BLACK,
                 graphic_param$blackmark, graphic_param$whitemark)
  y[cbind(x$unnumbered$y, x$unnumbered$x)] <- mark

  # insert numbers
  y[cbind(x$numbered$y, x$numbered$x)] <- as.character(x$numbered$move - origin)
  y[] <- sprintf("%3s", y)
  y <- apply(y, 1, paste0, collapse = "")


  # add vertical label
  y <- paste(sprintf("%3s| ", graphic_param$ylabels), y, sep = "")

  # flip y-axis so that the origin is at the left bottom
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
  ## if coordinates are out-of-bounds, show "pass"
  ## velow, v1 and v2 are x, y coordinates to print
  v1 <- rep("pass", nrow(x$noted)) # x coordinate
  v2 <- rep("", nrow(x$noted))     # y coordinate
  notPass <- (x$noted$x >= 1L & x$noted$y >= 1L &
                x$noted$x <= x$boardsize & x$noted$y <= x$boardsize)
  v1[notPass] <- graphic_param$xlabels[x$noted$x[notPass]]
  v2[notPass] <- graphic_param$ylabels[x$noted$y[notPass]]
  z <- sprintf("%s%d=%s%s",
               ifelse(x$noted$color == BLACK, "B", "W"),
               x$noted$move - origin, v1, v2)
  maxlen <- x$boardsize * 3L  # nchar per line
  cumlen <- cumsum(nchar(z) + 2)
  linenum <- floor(cumlen/maxlen)
  z <- data.frame(z, linenum, stringsAsFactors = FALSE) %>%
    dplyr::group_by_(~linenum) %>%
    dplyr::summarize_(~paste0(z, collapse = "; ")) %>%
    `[[`(2) %>%
    paste0(collapse = "\n  ")
  cat("\n ", z, "\n")
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
#' @return \code{\link{ggkifu}} object
#' @export
plot.gokifu <- function(x, y, ...)
{
  graphic_param <- set_graphic_param(boardsize = x$boardsize, ...)

  # set the new origin
  if (graphic_param$adjustorigin && !is.na(x$from)) {
    origin <- floor((x$from - 1) / 100) * 100
  } else {
    origin <- 0L
  }

  # board plot
  out1 <- ggoban(x$boardsize, ...) %>%
    # add initial stones
    # no error even if the data have data with no rows
    addstones(x = x$unnumbered$x, y = x$unnumbered$y, color = x$unnumbered$color,
              boardsize = x$boardsize, ...) %>%
    # add numbered stones
    addstones(x = x$numbered$x, y = x$numbered$y, color = x$numbered$color,
              number = x$numbered$move - origin,
              boardsize = x$boardsize, ...)

  # outside note
  if (nrow(x$noted) >= 1L) {
    out2 <- kifunote(x, ...)
  } else {
    out2 <- NULL
  }

  # compute the number of lines of outside note
  note_lines <- ceiling(nrow(x$noted) / graphic_param$moveperrow)

  # compute appropriate size ratio between board and note
  if (note_lines > 0) {
    # magic formula for computing the height
    fact <- graphic_param$endogenous$notestonesize / 5.25 * 0.18
    note_height <- (fact * note_lines + 0.42) * graphic_param$targetwidth / 5
  } else {
    note_height <- 0
  }

  board_width <- attr(out1, "savesize")[1]
  board_height <- attr(out1, "savesize")[2]
  heights <- c(board_height, note_height)

  # compute suggested size for saving
  savesize <- c(width = board_width, height = sum(heights))


  return(ggkifu(board = out1, note = out2, boardsize = x$boardsize,
                heights = heights, savesize = savesize))
}


#' Check if the object is gokifu class
#' @param x R object
#' @return logical
#' @export
is.gokifu <- function(x)
{
  return(inherits(x, "gokifu"))
}


#' Draw outside note of kifu
#' @param x \code{gokifu} object
#' @param ... graphic parameter
#' @return \code{ggplot} object
#' @export
kifunote <- function(x, ...)
{
  if (!is.gokifu(x)) stop("object is not gokifu class")

  graphic_param <- set_graphic_param(boardsize = x$boardsize, ...)

  k <- graphic_param$moveperrow
  colsize <- 4

  # set the new origin
  if (graphic_param$adjustorigin && !is.na(x$from)) {
    origin <- floor((x$from - 1) / 100) * 100
  } else {
    origin <- 0L
  }


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
  out <- addstones(out, xx, yy, x$noted$color, x$noted$move - origin,
                   stonesize  = graphic_param$endogenous$notestonesize,
                   numbersize = graphic_param$endogenous$notenumbersize,
                   # ensure that the stones are printed in the note
                   xlim = range(xx), ylim = range(yy),
                   # size is not controlled automatically
                   adjustsizeonboard = FALSE, ...)

  # add text
  ## coordinate to show
  ## v1, v2 correspond to x and y.
  ## if pass, then show 'pass'
  v1 <- rep("pass", nrow(x$noted)) # x coordinate
  v2 <- rep("", nrow(x$noted))     # y coordinate
  notPass <- (x$noted$x >= 1L & x$noted$y >= 1L &
                x$noted$x <= x$boardsize & x$noted$y <= x$boardsize)
  v1[notPass] <- graphic_param$xlabels[x$noted$x[notPass]]
  v2[notPass] <- graphic_param$ylabels[x$noted$y[notPass]]

  ll <- paste("    ", v1, v2, sep = "")
  dat <- data.frame(xx, yy, ll)
  out <- out +
    ggplot2::geom_text(data = dat,
                       ggplot2::aes_string(x = "xx", y = "yy", label = "ll"),
                       color = graphic_param$notetextcolor,
                       size = graphic_param$endogenous$notetextsize,
                       hjust = 0)

  attr(out, "graphic_param") <- graphic_param
  return(out)
}

