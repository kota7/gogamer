#' Find the tag properties in sgf text.
#'
#' @param sgf   A character string of sgf format
#' @param tags  A character vector of tags
#'
#' @return A character vector of the same size as \code{tags}.
#'   It contains the properties of tags (\code{NA} if the tag is missing)
#'   and is named by the tag names.
#'
#' @details This function finds the first appearance of each tag
#'   and returns the property (contents within the bracket).
#'   It suits for finding propeties unique to a game
#'   (e.g. meta information, date, players, rule set),
#'   but not for extracting tags appearing for multiple times, such as moves.
#'
#' @examples
#' sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
#' find_tags(sgf, "PB")
#' find_tags(sgf, c("PW", "PB", "EV", "RO"))
#'
#' @export
get_props <- function(sgf, tags) {
  # TODO:
  #   - regex would not work if brackets are in within the property
  #   - regex() should be defined somewhere as global variables
  stringr::str_match(
    sgf, paste(tags, "\\[(.*?)\\]", sep = ""))[, 2] %>%
    stats::setNames(tags)
}



#' Obtain game sequences (plays, comments, and times) in sgf text.
#'
#' @param sgf  a character of text in sgf format
#'
#' @return an integer matrix with five columns.
#' @export
get_moves <- function(sgf) {
  # TODO: regex() should be done somewhere and stored as global variable
  tmp <- stringr::str_match_all(sgf, stringr::regex(
    "(?<![A-Z])(AB|AW|B|W|BL|WL|C)((\\[.*?(?<!\\\\)\\])+)", dotall = TRUE))[[1]]
  tags <- tmp[, 2]
  props <- stringr::str_match_all(
    tmp[, 3], stringr::regex("\\[(.*?)(?<!\\\\)\\]", dotall = TRUE))

  # make a master dataframe
  DF <- Map(function(tag, prop) data.frame(tag = tag, prop = prop[, 2],
                                           stringsAsFactors = FALSE),
            tags, props) %>%
    dplyr::rbind_all() %>%
    dplyr::mutate(isMove = as.integer(tag %in% c("B", "W")),
                  move_num = cumsum(isMove))

  tmp <- DF %>% dplyr::filter(tag %in% c("B", "W", "AB", "AW")) %>%
    dplyr::mutate(colour = 1L + (tag=="W"))
  moves <- with(tmp, lapply(prop, utf8ToInt)) %>% unlist() %>% `-`(96L) %>%
    matrix(ncol = 2, byrow = TRUE) %>% as.data.frame() %>%
    setNames(c("x", "y")) %>%
    dplyr::bind_cols(tmp) %>%
    dplyr::select(move_num, x, y, colour, isMove)

  comments <- DF %>% dplyr::filter(tag == "C") %>%
    dplyr::select(move_num, comment = prop)

  times <- DF %>% dplyr::filter(tag %in% c("WL", "BL")) %>%
    dplyr::mutate(colour = 1L + (tag == "WL")) %>%
    dplyr::select(move_num, colour, time_left = prop)

  list(moves = moves, comments = comments, times = times)
}

