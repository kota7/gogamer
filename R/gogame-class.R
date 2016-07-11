### S3 class 'gogame' is defined ###


#' Go game object
#' @description  \code{gogame} stores go game record including
#' game plays, results and
#' other information such as player names and game setup.
#' @param properties  a list of game properties
#' @param moves  a data frame of game moves
#' @return \code{gogame} object
#' @export
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


  ### clean komi
  # if not specified, assume komi is zero
  if (is.na(properties$komi)) properties$komi <- 0L
  # if there komi is interpretable as numeric, then convert to numeric
  if (regexpr("^[[:space:]]*[\\-\\.0-9]+[[:space:]]*$", properties$komi) > 0)
    properties$komi <- as.numeric(properties$komi)


  ### clean handicap
  if (!is.na(properties$handicap)) {
    # convert to integer
    properties$handicap <- gsub("[^0-9]+", "", properties$handicap) %>%
      as.integer()
    # check the handicap property is consitent with the moves
    if (properties$handicap != sum(!moves$ismove))
      warning("handicap property does not equal the number of setup stones, ",
              properties$handicap, " vs ", sum(!moves$ismove))
  } else {
    properties$handicap <- sum(!moves$ismove)  # count the number of setups
  }

  ### flip the y axis so that bottom-left corner is the origin
  # this is consistent with labeling convention in major software
  # including Quarry and CGoban
  # this is valid for SGF format, but may not be for other formats
  moves$y <- boardsize - moves$y + 1L


  ### obtain board state transition
  transition <- get_transitions(
    boardsize, moves$ismove, moves$x, moves$y, moves$color)

  return(structure(
    .Data = c(properties, list(transition = transition)), class = "gogame"))
}



#' @export
print.gogame <- function(x, ...)
{
  cat("\n* Go game *\n\n")

  cat(" White : ")
  if (!is.na(x$whitename)) cat(x$whitename)
  if (!is.na(x$whiterank)) cat(sprintf(" (%s)", as.character(x$whiterank)))
  cat("\n")

  cat(" Black : ")
  if (!is.na(x$blackname)) cat(x$blackname)
  if (!is.na(x$blackrank)) cat(sprintf(" (%s)", as.character(x$blackrank)))
  cat("\n")

  cat(" Result: ")
  if (!is.na(x$result)) {
    cat(x$result)
  } else {
    cat("Unknown")
  }
  maxnum <- max(x$transition$move)
  if (is.integer(maxnum)) cat(sprintf(" (%d moves)", maxnum))
  cat("\n")

  cat("\n")
  cat(sprintf(" %-12s: %s\n", "komi", as.character(x$komi), "\n"))
  cat(sprintf(" %-12s: %s\n", "handicap", as.character(x$handicap), "\n"))
  cat(sprintf(" %-12s: %s\n", "board size", as.character(x$boardsize), "\n"))

  if (!is.na(x$rule))
    cat(sprintf(" %-12s: %s\n", "rule", as.character(x$rule), "\n"))
  if (!is.na(x$date))
    cat(sprintf(" %-12s: %s\n", "date", as.character(x$date), "\n"))
  if (!is.na(x$event))
    cat(sprintf(" %-12s: %s\n", "event", as.character(x$event), "\n"))
  if (!is.na(x$round))
    cat(sprintf(" %-12s: %s\n", "round", as.character(x$round), "\n"))
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




### following functions are not registered as generic method for the class,
### but assumes the arguments are the gogame class object



#' Go board status at a move number
#' @description This function obtains the board state at the move number.
#' The result is stored in a \code{\link{gostate}} object.
#' @param x \code{gogame} object
#' @param at integer of the move number
#' @return \code{\link{gostate}} object
#' @export
stateat <- function(x, at)
{
  if (!(is.gogame(x))) stop("object is not a gogame")

  # the following data frame represent the board state in
  # dense matrix format
  board <- x$transition %>%
    dplyr::filter_(~move <= at) %>%
    dplyr::group_by_(~x, ~y) %>%
    dplyr::summarize_(value = ~sum(value)) %>%
    dplyr::filter_(~value > 0L) %>%
    dplyr::rename_(color = ~value)

  # compute the number of prisoners
  capt <- x$transition %>%
    dplyr::filter_(~move <= at, ~value < 0L) %>%
    dplyr::group_by_(~value) %>%
    dplyr::summarize_(captured = ~length(move))

  b_captured <- 0L
  flg <- (capt$value == -BLACK)
  if (any(flg)) b_captured <- capt$captured[flg]

  w_captured <- 0L
  flg <- capt[["value"]] == -WHITE
  if (any(flg)) w_captured <- capt$captured[flg]

  # find the last move
  dat <- x$transition %>%
    dplyr::filter_(~move <= at, ~move >= 1L, ~value > 0L) %>%
    dplyr::arrange_(~move) %>% utils::tail(1)
  if (nrow(dat) == 1L) {
    lastmove <- c(dat$x, dat$y, dat$value)
  } else {
    lastmove <- NULL
  }

  out <- gostate(board, boardsize = x$boardsize,
                 b_captured = b_captured, w_captured = w_captured,
                 lastmove = lastmove)
  return(out)
}


#' Plot the go board state by ggplot
#' @param x \code{gogame} object
#' @param at Move number (integer)
#' @param ... arguments passed to \code{\link{print.gostate}}
#' @return \code{ggplot} object
#' @export
#' @examples
#' plotat(mimiaka, 127)
plotat <- function(x, at, ...)
{
  if (!(is.gogame(x))) stop("object is not a gogame")

  out <- stateat(x, at) %>%
    graphics::plot(...)

  return(out)
}



#' Kifu for certain move range
#' @param x \code{gogame} object
#' @param from,to integers specifying range of move
#' @return \code{\link{gokifu}} object
#' @export
#' @examples
#' data(saikoyo)
#' kifu(saikoyo)
kifu <- function(x, from = 1L, to = 100L)
{
  if (!(is.gogame(x))) stop("object is not a gogame")

  out <- x %>%
    # obtain the board state just before 'from'
    # and define the move number as 0
    # this part is regarded as the 'initial' state of the kifu
    stateat(from - 1L) %>% `[[`("board") %>%
    dplyr::mutate(move = 0L) %>%
    # then append the moves between 'from' to 'to'
    # note that moves are the ones with positive values
    # these are candidate move numbers to be shown in the kifu
    dplyr::bind_rows(dplyr::filter_(
      x[["transition"]], ~move >= from, ~move <= to, ~value > 0L) %>%
        dplyr::rename_(color = ~value)) %>%
    dplyr::arrange_(~move)

  # finally, for each (x, y), the first entry (smallest move number)
  # is the ones to be marked in the kifu
  # the other moves are listed outside
  flg <- !duplicated(dplyr::select_(out, ~x, ~y))

  # define three separate data frames
  # init      ... initial board state (shown on the board, no number)
  # numbered  ... moves to be shown on the board with number
  # noted     ... moves to be listed outside
  init     <- dplyr::filter_(out, ~move == 0L)
  numbered <- dplyr::filter_(out, ~move != 0L, ~flg)
  noted    <- dplyr::filter_(out, ~move != 0L, ~!flg)


  out <- gokifu(init = init, numbered = numbered, noted = noted,
                boardsize = x$boardsize, from = from, to = to)
  return(out)
}


