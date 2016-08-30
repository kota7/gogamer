### S3 class 'gogame' is defined ###


#' Go game object
#' @description  \code{gogame} stores go game record including
#' game plays, results and
#' other information such as player names and game setup.
#' @param properties  a list of game properties
#' @param gametree    a list
#' @return \code{gogame} object
#' @export
gogame <- function(properties, gametree)
{
  ## input validity check
  if (!is.list(properties)) stop("properties must be a list")
  if (!is.list(gametree)) stop("hametree must be a list")


  ## clean properties
  ### impute missing properties with NA
  prop_names <- c("whitename", "whiterank", "blackname", "blackrank",
                  "boardsize", "komi", "handicap", "date", "event", "round",
                  "result", "rule", "place") %>%
    setdiff(names(properties))
  properties[prop_names] <- NA_character_
  ### boardsize
  properties$boardsize <- guess_boardsize(properties$boardsize, 0L)
  ### clean komi
  # if not specified, assume komi is zero
  if (is.na(properties$komi)) {
    properties$komi <- 0L
  } else if (is.character(properties$komi)) {
    # if there komi is interpretable as numeric, then convert to numeric
    if (grepl("^[[:space:]]*[\\-\\.0-9]+[[:space:]]*$", properties$komi))
      properties$komi <- as.numeric(properties$komi)
  }
  ### clean handicap
  # Decided NOT to
  # - check if the handicap is consistent with the number of setup moves
  # - guess and fill the handicap propperty based on the number of setup moves
  if (is.na(properties$handicap)) {
    properties$handicap <- 0L
  } else if (!is.na(properties$handicap)) {
    # convert to integer
    properties$handicap <- grep("[0-9]+", properties$handicap, value = TRUE) %>%
      as.integer()
  }


  ## check game tree
  ### if transition is missing and move exists, then
  ### compute the transition using move children (children may possibly be NULL)
  if (!("transition" %in% names(gametree)) && ("move" %in% names(gametree)))
    gametree$transition <- get_transition_wrapper(gametree$move,
                                                  gametree$children)
  ### if parent, children, leaf are missing,
  ### impute as much as possible
  ### usually, all three should be given, or none of them is given
  check <- c("parent", "children", "leaf") %in% names(gametree)
  if (!check[1] && !check[2]) {
    # both parent and children missing -> assume only one node
    gametree$parent   <- 0L
    gametree$children <- list(integer(0))
  } else if (check[1] && !check[2]) {
    # impute children from parent
    gametree$children <- lapply(seq_along(gametree$parent),
                                function(i) which(i == gametree$parent))
  } else if (!check[1] && check[2]) {
    # impute parent from children
    len <- lapply(gametree$children, length) %>% unlist()
    index <- Map(rep, seq_along(len), len) %>% unlist()
    gametree$parent <- rep(NA_integer_, length(gametree$children))
    gametree$parent[unlist(gametree$children)] <- index
    gametree$parent[1] <- 0L
  }
  if (!check[3]) {
    # impute leaf from children
    gametree$leaf <- seq_along(gametree$parent) %>% setdiff(gametree$parent)
  }

  out <- structure(.Data = c(properties, list(gametree = gametree)),
                   class = "gogame")
  out <- set_branch(out, 1L)
  return(out)
}


#' @export
print.gogame <- function(x, ...)
{
  cat("* Go game *\n\n")

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
  if (!is.na(x$handicap))
    cat(sprintf(" %-12s: %s\n", "handicap", as.character(x$handicap), "\n"))
  cat(sprintf(" %-12s: %s\n", "board size", as.character(x$boardsize), "\n"))

  if (!is.na(x$rule))
    cat(sprintf(" %-12s: %s\n", "rule", as.character(x$rule), "\n"))
  if (!is.na(x$date))
    cat(sprintf(" %-12s: %s\n", "date", as.character(x$date), "\n"))
  if (!is.na(x$place))
    cat(sprintf(" %-12s: %s\n", "place", as.character(x$place), "\n"))
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
#' @examples
#' stateat(saikoyo, 116)
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

  if (at >= max(x$transition$move)) {
    points <- x$points
  } else {
    points <- NULL
  }

  out <- gostate(board, boardsize = x$boardsize,
                 b_captured = b_captured, w_captured = w_captured,
                 lastmove = lastmove, points = points)
  return(out)
}


#' Plot the go board state by ggplot
#' @param x \code{gogame} object
#' @param at Move number (integer)
#' @param ... arguments passed to \code{\link{plot.gostate}}
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
#' @param from,to  Positive integers. Range of moves
#' @param restart  Positive integer. If supplied, this number is used as the
#' smallest move number in the range. If not supplied, original move numbers
#' are used as they are.
#' @return \code{\link{gokifu}} object
#' @export
#' @examples
#' kifu(saikoyo)
kifu <- function(x, from = 1L, to = 100L, restart = NA_integer_)
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
  numbered <- dplyr::filter_(out[flg,], ~move != 0L)
  noted    <- dplyr::filter_(out[!flg,], ~move != 0L)

  # replace the move numbers by the specified first number
  if (!is.na(restart)) {
    moves <- c(numbered$move, noted$move)
    if (length(moves) > 0L) {
      deviation <- min(moves) - restart
      numbered$move <- numbered$move - deviation
      noted$move <- noted$move - deviation
    }
  }

  out <- gokifu(init = init, numbered = numbered, noted = noted,
                boardsize = x$boardsize)
  return(out)
}



#' Switch branch of go game
#' @param x  \code{gogame} object
#' @param branch integer
#'
#' @return \code{gogame} object
#' @export
set_branch <- function(x, branch = 1L)
{
  if (!(is.gogame(x))) stop("object is not a gogame")
  if (branch > length(x$gametree$leaf))
    stop("this game has only ", length(x$gametree$leaf), " branch(es)")

  nodes <- get_branchpath(x$gametree$parent, x$gametree$leaf[branch])
  x$transition <- dplyr::filter_(x$gametree$transition, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)
  x$point <- dplyr::filter_(x$gametree$point, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)
  x$comment <- dplyr::filter_(x$gametree$comment, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)

  return(x)
}

