### S3 class 'gogame' is defined ###


#' Go game object
#' @description  \code{gogame} class object capusulizes a go game record.
#' It keeps stores such game plays, outcomes as well as
#' other information such as player names and date.
#' The object supports various methods for creating board images and
#' Kifu documents.
#' @param properties  a list of game properties
#' @param gametree    a list
#' @return \code{gogame} object
#' @export
#' @seealso \code{\link{read_sgf}}, \code{\link{parse_sgf}},
#' \code{\link{plotat}}, \code{\link{stateat}}, \code{\link{kifu}}, \code{\link{kifuplot}}
#' @details This is a constructor of \code{gogame} class object.
#' It is mainly designed to be called from \code{\link{parse_sgf}} function,
#' which interprets text of sgf format and creates the corresponding
#' \code{gogame} object.
#'
#' The object can produce two kinds of game images.
#' The first is a board snapshot at an arbitrary timing.
#' \code{\link{stateat}} is for computing the board configuration and
#' \code{\link{plotat}} is for drawing the board image.
#' The second kind of images is the kifu document that summarize a range of
#' moves in a page.
#' \code{\link{kifu}} is prepared for computing the move sequence and
#' \code{\link{kifuplot}} is for drawing the image.
#'
#' The constructor takes two mandatory arguments: \code{properties} and \code{gametree}.
#' \code{properties} is a named list of game meta information.
#' Currently, following names are recognized (variable type in parentheses).
#' One may provide additional information so long as it does not contradict
#' names of other information. Check \code{str(mimiaka)} to see a list of
#' variable names already in use.
#' \describe{
#' \item{\code{boardsize}}{board size (\code{integer}}
#' \item{\code{whitename}, \code{whiterank}, \code{blackname}, \code{blackrank}}{player
#' names and ranks (\code{character})}
#' \item{\code{komi}}{Komi (\code{numeric})}
#' \item{\code{handicap}}{number of handicap stones (\code{integer})}
#' \item{\code{result}}{game outcome (\code{character})}
#' \item{\code{date}, \code{place}}{game date and location (\code{character})}
#' \item{\code{event}, \code{round}}{competition or event name and round (\code{character})}
#' \item{\code{rule}}{rule (\code{character})}
#' }
#'
#' \code{gametree} stores game plays, setups, comments, and territory counts.
#' To accomodate the cases of games with branches, the object employs a tree
#' data structure.
#' \code{gametree} should contain potentially four data sets.
#' Parentheses list the variable names to be contained.
#' \describe{
#' \item{\code{transition}}{Transition of board configuration.
#' a positive value means a stone is added, negative means removed.
#' Absolute values of value indicate the stone color (1: black, 2: white)
#' (\code{x}, \code{y}, \code{value}, \code{move}, \code{nodeid})}
#' \item{\code{move}}{Moves and setups. \code{ismove} indicates whether it is a
#' move or setup (\code{x}, \code{y}, \code{color}, \code{ismove}, \code{move}, \code{nodeid})}
#' \item{\code{point}}{Territories.
#' (\code{x}, \code{y}, \code{color}, \code{move}, \code{nodeid})}
#' \item{\code{comment}}{Comments made during the game.
#' (\code{comment}, \code{move}, \code{nodeid})}
#' }
#' \code{nodeid} variable in each data indicates the tree node to which the row belongs to.
#'
#' \code{gametree} also include three variables that describe tree structure.
#' \describe{
#' \item{\code{parent}}{integer vector that points parent of each node. The first node is always
#' assumed to be the root node and its parent is zero}
#' \item{\code{children}}{list of integer vector that points children of each node}
#' \item{\code{leaf}}{integer vector of leaf nodes (i.e. no children)}
#' }
#' For example, when there is only one node, \code{nodeid} variable of all data should be 1,
#' and \code{parent} is 0 (there is only the root node), \code{children} has unique entry
#' and it is integer vector of length zero, and \code{leaf} is 1.
#' When some of the three components are missing, then the function tries the best to
#' recover from the supplied information
#'
#' When \code{transition} is missing, then the function tries to compute it using
#' \code{move} and tree structure information.
#'
gogame <- function(properties, gametree)
{
  ## input validity check
  if (!is.list(properties)) stop("properties must be a list")
  if (!is.list(gametree)) stop("gametree must be a list")


  ## clean properties
  ### impute missing properties with NA
  prop_names <- c("whitename", "whiterank", "blackname", "blackrank",
                  "boardsize", "komi", "handicap", "date", "event", "round",
                  "result", "rule", "place") %>%
    setdiff(names(properties))
  properties[prop_names] <- NA_character_
  ### clean boardsize
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


  ## impute tree structure (parent, children, leaf), if any is missing
  ### In principle, one should supply
  ### wither all components or none of them
  ### If none of them is supplied, assumes there is only one node, which is 1
  ### If partial information is supplied, then the following function
  ### tries to impute the missing components
  gametree[c("parent", "children", "leaf")] <- do.call(
    fill_tree_structure, gametree[c("parent", "children", "leaf")])
  ### check consistency of tree structure
  check <- do.call(
    check_tree_structure,  gametree[c("parent", "children", "leaf")])
  if (!check) stop("invalid tree strucure")


  ## check game tree
  ### gametree includes the following data.frames:
  ###   transition: x, y, value, move, nodeid
  ###   move      : x, y, color, ismove, move, nodeid
  ###   point     : x, y, color, move, nodeid
  ###   comment   : comment, move, nodeid
  ### and following tree structure information:
  ###   parent  : integer vector
  ###   children: list of integer vector
  ###   leaf    : integer vector
  ###
  ### If transiion is missing, then we need to make one using the move
  if (!("transition" %in% names(gametree)) && !("move" %in% names(gametree)))
    stop("transition or move must be supplied in game tree")
  ### if transition is missing and move exists, then
  ### compute the transition using move children (children may possibly be NULL)
  if (!("transition" %in% names(gametree)) && ("move" %in% names(gametree)))
    gametree$transition <- get_transition_wrapper(
      gametree$move, gametree$children)
  ### valiable name check
  if (is.null(gametree$transition)) stop("game transition is missing")
  varnames <- c("x", "y", "value", "move", "nodeid")
  check <- varnames %in% names(gametree$transition)
  if (any(!check))
    stop("transition data must contain ", paste0(varnames, collapse = ", "))
  ### if point or comment is missing, then impute empty data.frame
  ### otherwise check variable names
  if (is.null(gametree$point)) {
    gametree$point <- data.frame(
      color = integer(0), x = integer(0), y = integer(0),
      move = integer(0), nodeid = integer(0))
  } else {
    varnames <- c("x", "y", "color", "move", "nodeid")
    check <- varnames %in% names(gametree$point)
    if (any(!check))
      stop("point data must contain ", paste0(varnames, collapse = ", "))
  }
  if (is.null(gametree$comment)) {
    gametree$comment <- data.frame(
      comment = character(0), move = integer(0), nodeid = integer(0),
      stringsAsFactors = FALSE)
  } else {
    varnames <- c("comment", "move", "nodeid")
    check <- varnames %in% names(gametree$comment)
    if (any(!check))
      stop("comment data must contain ", paste0(varnames, collapse = ", "))
  }


  ### remove points with out-of-bounds coordinates
  ### do not do the same for transition or move since OB represents pass
  gametree$point <- dplyr::filter_(gametree$point,
                                   ~x >= 1L, ~x <= properties$boardsize,
                                   ~y >= 1L, ~y <= properties$boardsize)


  ### compile output
  out <- structure(
    .Data = c(properties, list(gametree = gametree)), class = "gogame")
  out <- set_gamepath(out, 1L)

  ## store the move count of main branch (branch = 1)
  out$mainbranchmoves <- max(c(0L, out$transition$move))

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
  if (is.integer(x$mainbranchmoves))
    cat(sprintf(" (%d moves)", x$mainbranchmoves))
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

  if (length(x$gametree$leaf) > 1L) {
    cat(sprintf(
      "\n* currently at path %d / %d (%d moves)\n",
      x$pathid, length(x$gametree$leaf), max(c(0L, x$transition$move))))
  }
}


#' @export
as.list.gogame <- function(x, ...)
{
  return(x[])
}


#' Check if the object is gogame class
#' @param x R object
#' @return logical
#' @export
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
#' @return \code{stateat} returns a \code{\link{gostate}} object
#' @export
#' @examples
#' stateat(saikoyo, 116)
stateat <- function(x, at)
{
  if (!(is.gogame(x))) stop("object is not a gogame")


  boardsize <- x$boardsize
  # the following data frame represent the board state in
  # dense matrix format
  board <- x$transition %>%
    dplyr::filter_(~move <= at, ~x >= 1L, ~y >= 1L,
                   ~x <= boardsize, ~y <= boardsize) %>%
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

  # find the territories and comment,
  ## we look for the same move number as the last move above,
  ## except for the case with at is 0,
  ## where find the comment and point with move = 0
  if (at == 0L) {
    tgt_move <- 0L
  } else {
    tgt_move <- dat$move
  }
  points <- dplyr::filter_(x$point, ~move == tgt_move)
  comment <- x$comment$comment[x$comment$move == tgt_move]

  out <- gostate(board, boardsize = x$boardsize, movenumber = tgt_move,
                 b_captured = b_captured, w_captured = w_captured,
                 lastmove = lastmove, points = points, comment = comment)
  return(out)
}


#' @param ... arguments passed to \code{\link{plot.gostate}}
#' @return \code{plotat} returns a \code{ggplot} object
#' @export
#' @rdname stateat
#' @examples
#' plotat(mimiaka, 127)
plotat <- function(x, at, ...)
{
  if (!(is.gogame(x))) stop("object is not a gogame")

  stateat(x, at) %>% graphics::plot(...)
}



#' Kifu for certain move range
#' @param x \code{gogame} object
#' @param from,to  Positive integers. Range of moves
#' @param restart  Positive integer. If supplied, this number is used as the
#' smallest move number in the range. If not supplied, original move numbers
#' are used as they are.
#' @return \code{kifu} returns a \code{\link{gokifu}} object
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
  # also, if (x, y) is out of bounds then this is regarded as pass and
  # to be listed outside
  flg <- !duplicated(dplyr::select_(out, ~x, ~y)) &
    (out$x >= 1L & out$y >= 1L & out$x <= x$boardsize & out$y <= x$boardsize)

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


#' @param ... graphic parameters
#' @return \code{kifuplot} returns a \code{\link{ggkifu}} object
#' @export
#' @rdname kifu
#' @examples
#' kifuplot(mimiaka, 127, 150)
kifuplot <- function(x, from = 1L, to = 100L, restart = NA_integer_, ...)
{
  # one line wrapper for kifu -> plot
  if (!(is.gogame(x))) stop("object is not a gogame")

  kifu(x, from = from, to = to, restart = restart) %>% graphics::plot(...)
}


#' Switch path of go game
#' @description Switch path of a go game. Paths are indexed by integers starting
#' at one. If pathid exceeds the number of paths stored in the game, the function
#' throws an error.
#' @param x  \code{gogame} object
#' @param pathid integer
#'
#' @return \code{gogame} object
#' @export
set_gamepath <- function(x, pathid = 1L)
{
  if (!(is.gogame(x))) stop("object is not a gogame")
  if (pathid > length(x$gametree$leaf)) {
    mess <- c("this game has only ", length(x$gametree$leaf), " game path")
    if (length(x$gametree$leaf) > 1) mess[3] <- " paths"
    stop(paste0(mess, collapse = ""))
  }
  nodes <- get_branchpath(x$gametree$parent, x$gametree$leaf[pathid])
  x$transition <- dplyr::filter_(x$gametree$transition, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)
  #x$move <- dplyr::filter_(x$gametree$move, ~nodeid %in% nodes) %>%
  #  dplyr::arrange_(~move)
  x$point <- dplyr::filter_(x$gametree$point, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)
  x$comment <- dplyr::filter_(x$gametree$comment, ~nodeid %in% nodes) %>%
    dplyr::arrange_(~move)

  ## store the current branch id
  x$pathid <- as.integer(pathid)

  return(x)
}

