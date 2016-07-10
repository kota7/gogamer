### S3 class 'gogame' is defined ###


#' Go game object
#' @description  \code{gogame} stores go game record including
#' game plays, results and
#' other information such as player names and game setup.
#' @param properties  a list of game properties
#' @param moves  a data frame of game moves
#' @return \code{gogame} object
gogame <- function(properties, moves)
{
  ### get/clean/guess board size
  # first, check boardsize properties
  # needs a bit of cleaning to deal with
  # cases like "19x19"
  # so, extract the first consecutive digit
  # then, check the maximum number appearing in the move positions -> maxnum

  boardsize <- properties$boardsize %>%
    stringr::str_extract("[0-9]+") %>% as.integer()
  maxnum <- max(max(moves$x), max(moves$y))

  guess_flg <- FALSE
  if (is.na(boardsize)) {
    # guess the boardsize from 9, 13, 19
    cat("board size is not specified... will guess\n")
    guess_flg <- TRUE
  } else if (boardsize < maxnum) {
    warning("the maximum position exceeds the specified size... will guess")
    guess_flg <- TRUE
  }

  if (guess_flg) {
    # if the maximum position exceeds 19, error
    if (maxnum <= 9L) {
      boardsize <- 9L
    } else if (maxnum <= 13L) {
      boardsize <- 13L
    } else if (maxnum <= 19L) {
      boardsize <- 19L
    } else {
      stop("the maximum position exceeds 19... cannot guess the boardsize")
    }
    cat("boardsize is guess to be ", boardsize, "\n")
  }
  # update boardsize property
  properties$boardsize <- boardsize


  ### flip the y axis so that bottom-left corner is the origin
  # this is consistent with labeling convention in major software
  # including Quarry and CGoban
  # this is valid for SGF format, but may not be for other formats
  moves[["y"]] <- boardsize - moves[["y"]] + 1L


  ### obtain board state transition
  transition <- get_transitions(
    boardsize, moves$ismove, moves$x, moves$y, moves$color)

  return(structure(
    .Data = c(properties, list(transition = transition)), class = "gogame"))
}


#' @export
print.gogame <- function(x, ...)
{
  # TODO: define!
  cat("\n* Go game *\n")
  cat("***\n")
}


#' @export
as.list.gogame <- function(x, ...)
{
  return(x[])
}


#' @param x An R object
#' @return Logical.
#' @export
#' @rdname gogame
is.gogame <- function(x)
{
  return(inherits(x, "gogame"))
}


#' Go board status at a move number
#' @description This function obtains the board state at the move number.
#' The result is stored in a \code{\link{gostate}} object.
#' @param x \code{gogame} object
#' @param at integer of the move number
#' @return \code{\link{gostate}} object
#' @export
stateat <- function(x, at)
{
  if (!(is.gogame(x))) stop("object it not a gogame")

  # the following data frame represent the board state in
  # dense matrix format
  board <- x$transition %>%
    dplyr::filter(move <= at) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::filter(value > 0L) %>%
    dplyr::rename(color = value)

  # compute the number of prisoners
  capt <- x$transition %>%
    dplyr::filter(move <= at, value < 0L) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(captured = length(move))

  b_captured <- 0L
  flg <- capt[["value"]] == -BLACK
  if (any(flg)) b_captured <- capt$captured[flg]

  w_captured <- 0L
  flg <- capt[["value"]] == -WHITE
  if (any(flg)) w_captured <- capt$captured[flg]

  out <- gostate(board, boardsize = x$boardsize,
                 b_captured = b_captured, w_captured = w_captured)
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
                   marklast = TRUE, lastmarker = utf8ToInt(9650), ...)
{
  stopifnot("gogame" %in% class(x))

  out <- stateat(x, at) %>% plot(...) # draw stone allocation

  # add marker to the last move
  if (marklast) {
    dat2 <- dplyr::filter(x$transition, move <= at, move >= 1L, value > 0L) %>%
      dplyr::arrange(move) %>% utils::tail(1)
    if (nrow(dat2) == 1L)
      out <- addlabels(out, dat2$x, dat2$y, lastmarker, dat2$value)
  }
  return(out)
}


