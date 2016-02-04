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
find_tags <- function(sgf, tags) {
  stringr::str_match(
    sgf, paste(tags, "\\[(.*?)\\]", sep = ""))[, 2] %>%
    stats::setNames(tags)
}



#' Obtain moves in sgf text.
#'
#' @param sgf  a character of text in sgf format
#'
#' @return an integer matrix with five columns.
#' @export
get_moves <- function(sgf) {
  tmp <- stringr::str_extract_all(sgf, "[^A-Z][A]*[BW](\\[.*?\\])+",
                                  simplify = TRUE)
  tags <- stringr::str_match(tmp, "[A-Z]+")
  locs <- stringr::str_match_all(tmp, "\\[(.*?)\\]")

  # colour
  df <- Map(function(tag, loc) data.frame(tag = tag, loc = loc[, 2],
                                    stringsAsFactors = FALSE), tags, locs) %>%
    dplyr::rbind_all() %>%
    dplyr::mutate(colour = 1L + stringr::str_detect(tag, "W$"),
                  isMove = 2L - stringr::str_length(tag),
                  move = cumsum(isMove))
  df2 <- lapply(df$loc, utf8ToInt) %>%
    plyr::ldply(function(x) x - 96L) %>%
    setNames(c("x", "y"))

  dplyr::bind_cols(df2,
    dplyr::select(df, colour, move, isMove)) %>% as.matrix()
}