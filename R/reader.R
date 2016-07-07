
#' Read and parse a SGF file
#'
#' @name read_sgf
#' @param path    character string of the path to
#'   a smart go format (SGF) file.  Can be local or online.
#' @param keep_first logical indicating the branch choice rule
#' @param encoding  character string.
#'   If specified, declares the encoding used on a file.
#'
#' @export
read_sgf <- function(path, keep_first = TRUE, encoding = "") {
  readLines(path, encoding = encoding) %>%
    paste0(collapse = "\n") %>%
    parse_sgf(keep_first)
}



#' Parse text of the smart go format.
#'
#' @name parse_sgf
#' @param sgf     scalar character of sgf text.
#' @param keep_first  logical specifying the branch choice rule.
#'   If TRUE, keep the first appearing branch.
#'   Otherwise, keep the last branch.
#'
#' @export
parse_sgf <- function(sgf, keep_first = TRUE) {
  ### obtain meta information ###
  tags <- c("PW", "PB", "WR", "BR",
            "RE", "SZ", "KM", "HA",
            "DT", "RU", "EV", "RO")
  props <- get_props(sgf, tags)

  ### parse plays, comments, and times ###
  sgf <- prune_sgf(sgf, keep_first)
  moves <- get_moves(sgf)

  ### get board size
  # first, check SZ element
  # needs a bit of cleaning to deal with
  # cases like "19x19"
  # so, extract the first consecutive digit -> boardsize
  # then, check the maximum number appearing in the move positions -> maxnum

  boardsize <- props[["SZ"]] %>%
    stringr::str_extract("[0-9]+") %>% as.integer()
  maxnum <- max(max(moves[["x"]]), max(moves[["y"]]))

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

  # add the boardsize to the propetry list
  props[["boardsize"]] <- boardsize


  ### flip the y axis so that bottom-left corner is the origin
  # this is consistent with labeling convention in major software
  # including Quarry and CGoban
  moves[["y"]] <- boardsize - moves[["y"]] + 1L


  ### obtain board state transition
  transition <- get_transitions(
    boardsize, moves[["ismove"]], moves[["x"]], moves[["y"]], moves[["color"]]
  )

  return(
    structure(.Data = c(props, list(transition = transition)),
              class = "gogame")
  )
}
