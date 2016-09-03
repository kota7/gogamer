### tag parsers ###

#' Find tag properties in sgf
#'
#' @param sgf   scalar character of texts in sgf format
#' @param tags  character vector of tags
#'
#' @return  Named list of game properties
#'
#' @details This function finds the first appearance of each tag
#'   and returns the property (contents within the bracket).
#'   It suits for finding propeties unique to a game
#'   (e.g. player names or rule set),
#'   but not for extracting tags appearing for multiple times, such as moves.
#'
#' @keywords internal
get_props <- function(sgf, tags) {
  if (length(sgf) > 1) {
    warning("length(sgf) > 1: only the first element is used")
    sgf <- sgf[1]
  }

  out <- sprintf("(?<![A-Z])%s\\[(.*?)(?<!\\\\)\\]", tags) %>%
    lapply(function(p) stringr::str_match(sgf, p)[, 2]) %>%
    stats::setNames(tags)

  return(out)
}



#' Parse SGF of each node
#' @param sgf A character vector of SGF nodes
#' @param asList if this is True, each component is list of the node size
#'
#' @return A list
#' @keywords internal
parse_sgfnode <- function(sgf)
{

  ## replace "\\\\]" to a temporal replacement
  ### I found it a bit hard to write an regex for skipping "\]" until the
  ### next "]", so decided to replace "\]" by some rarely used letters first,
  ### extract comments, then revert "\]" finally
  ### Note that "(?<![A-Z])(C)\\[(.*?)\\]" does not catch line breaks
  ### within comments, leading to using "(?<![A-Z])(C)\\[([^\\]]*?)\\]" instead
  ## pick a temporary replacement
  i <- 1L
  tmp_rep <- intToUtf8(i)
  while (any(grepl(tmp_rep, sgf))) {
    i <- i + 1
    tmp_rep <- paste(tmp_rep, intToUtf8(i), sep = "")
    if (i >= 100) stop("the text has too many weird characters")
  }
  sgf <- stringr::str_replace_all(sgf, "\\\\]", tmp_rep)



  ## loop over tags
  ### decided not to 'vectorize' since it can be memory intensive
  ### for very long sgf
  tags <- c("B", "W", "AB", "AW", "AE",
            "TB", "TW", "C", "LB",
            "MA", "TR", "CR", "SQ")
  idvec   <- integer(0)
  textvec <- character(0)
  tagvec  <- character(0)
  for (tag in tags)
  {
    rr <- sprintf("(?<![A-Z])(%s)(\\[([^\\]]*?)\\])+", tag)
    tmp <- stringr::str_match(sgf, rr)
    ## tmp is a character matrix of size [len(data), 4]
    ## col 2 is the tag, col 1 is the full matched component
    ## col 3 and col 4 are not necessary

    ## remove rows with NA, meaning no match
    ## keep the indx pointer in the idvec
    flg <- !is.na(tmp[, 1])
    if (any(flg)) {
      idvec   <- c(idvec, which(flg))
      textvec <- c(textvec, tmp[flg, 1])
      tagvec  <- c(tagvec, tmp[flg, 2])
    }
  }
  ## remove tag from text
  ## so the textvec consists only of brackets
  textvec <- stringr::str_replace(textvec, tagvec, "")

  ## expand multiple brackets
  ### e.g. AB[dd][qq:st]
  ### e.g. LB[dd:3][ed:1][fd:2][df:c][ef:a][ff:b]
  ### note. there may be empty content, []
  tmp <- stringr::str_extract_all(textvec, "\\[[^\\]]*?\\]")
  len <- sapply(tmp, length)
  ### expand id and tags accordingly
  index <- Map(rep, seq_along(len), len) %>% unlist()
  idvec   <- idvec[index]
  tagvec  <- tagvec[index]
  textvec <- unlist(tmp)
  textvec <- substring(textvec, 2, nchar(textvec) - 1)
  ### surrounding brackets are removed by this
  ### revert the temporal replacement
  textvec <- stringr::str_replace_all(textvec, tmp_rep, "\\\\]")
  rm(tmp, index, tmp_rep)


  ## moves (B, W)
  flg  <- tagvec %in% c("B", "W")
  if (any(flg)) {
    text <- textvec[flg]
    tag  <- tagvec[flg]
    id   <- idvec[flg]
    ### check for unrecognized text and warn
    ### content must be 2-alphabets or empty
    check <- grepl("^[a-z]{2}$", text) | (text == "")
    if (!all(check))
      warning("these moves are unrecognized move and converted to 'pass' ",
              text[!check])
    ### empty or unrecognized texts are converted to '00' (letter prior to 'a')
    text[text == "" | !check] <- "\u60\u60"
    x <- substring(text, 1, 1) %>% char_to_coord()
    y <- substring(text, 2, 2) %>% char_to_coord()
    out_move <- data.frame(
      id = id, color = ifelse(tag == "W", WHITE, BLACK),
      x = x, y = y, ismove = TRUE)
  } else {
    out_move <- data.frame(
      id = integer(0), color = integer(0),
      x = integer(0), y = integer(0), ismove = logical(0))
  }
  ####


  ## parse tags with rectangle expressions
  ## i.e., 'add stones' (AB, AW, AE)
  ##       'territory'  (TB, TW)
  ##       'markers'    (MA, CR, SQ, TR)
  flg <- tagvec %in% c("AB", "AW", "AE", "TB", "TW",
                       "MA", "SQ", "TR", "CR")
  if (any(flg)) {
    text <- textvec[flg]
    tag  <- tagvec[flg]
    id   <- idvec[flg]
    ### check for unrecognized text and warn
    ### must be aa or aa:aa format
    check <- grepl("^[a-z]{2}$", text) | grepl("^[a-z]{2}:[a-z]{2}", text)
    if (!all(check)) {
      warning("these coordinate expressions are not recognized and ignored",
              text[!check])
      text <- text[check]
      id   <- id[check]
      tag  <- tag[check]
    }
    ### expand rectangle expression: bb:cd -> bb, bc, bd, cb, cc, cd
    #### this function returns a data frame with (x, y, index)
    #### where index maps the row to the index of the text
    tmp <- expand_rectangle(text)

    out_rect <- data.frame(id = id[tmp$index], tag = tag[tmp$index],
                           x = tmp$x, y = tmp$y)
  } else {
    out_rect <- data.frame(id = integer(0), tag = character(0),
                           x = integer(0), y = integer(0))
  }
  ### split into 'add stone', 'points', and 'makers'
  out_add <- dplyr::filter_(out_rect, ~tag %in% c("AB", "AW", "AE"))
  if (nrow(out_add) > 0L) {
    out_add$color <- EMPTY
    out_add$color[out_add$tag == "AB"] <- BLACK
    out_add$color[out_add$tag == "AW"] <- WHITE
    out_add$ismove <- FALSE
  } else {
    out_add$color <- integer(0)
    out_add$ismove <- logical(0)
  }
  out_add <- out_add %>% dplyr::select_(~ -tag)

  out_point <- dplyr::filter_(out_rect, ~tag %in% c("TB", "TW"))
  if (nrow(out_point) > 0L) {
    out_point$color[out_point$tag == "TB"] <- BLACK
    out_point$color[out_point$tag == "TW"] <- WHITE
  } else {
    out_point$color <- integer(0)
  }

  out_marker <- dplyr::filter_(out_rect, ~tag %in% c("MA", "CR", "SQ", "TR"))
  if (nrow(out_marker) > 0L) {
    out_marker$marker <- NA_integer_
    out_marker$marker[out_marker$tag == "SQ"] <- 0L
    out_marker$marker[out_marker$tag == "CR"] <- 1L
    out_marker$marker[out_marker$tag == "TR"] <- 2L
    out_marker$marker[out_marker$tag == "MA"] <- 4L
  } else {
    out_marker$marker <- integer(0)
  }
  out_marker <- out_marker %>% dplyr::select_(~ -tag)
  ####


  ## parse comments (C)
  flg <- tagvec %in% "C"
  out_comment <- data.frame(
    id = idvec[flg], comment = textvec[flg], stringsAsFactors = FALSE)
  ####

  ## parse label (LB)
  flg <- tagvec %in% "LB"
  text <- textvec[flg]
  tag  <- tagvec[flg]
  id   <- idvec[flg]
  if (any(flg)) {
    ### check for unrecognized text and warn
    ### content must be 2-alphabets + : + label
    check <- grepl("^[a-z]{2}:.*$", text) | (text == "")
    if (!all(check)) {
      warning("these label expressions are not recognized and ignored",
              text[!check])
      text <- text[check]
      id   <- id[check]
      tag  <- tag[check]
    }
    x <- char_to_coord(substring(text, 1, 1))
    y <- char_to_coord(substring(text, 2, 2))
    label <- substring(text, 4)
    out_label <- data.frame(x = x, y = y, label = label,
                            stringsAsFactors = FALSE)
  } else {
    out_label <- data.frame(x = integer(0), y = integer(0),
                            label = character(0),  stringsAsFactors = FALSE)
  }

  ## finalizing outputs
  ### moves and setups are combined and sorted by id
  moves <- dplyr::bind_rows(out_add[names(out_add)],
                            out_move) %>%
    dplyr::arrange_(~ id)

  return(list(
    moves = moves, points = out_point, comments = out_comment,
    makers = out_marker, labels = out_label))
}

