#' Find the tag properties in sgf text.
#'
#' @param sgf  A character
#' @param tags A character vector of tags
#'
#' @return A character vector of the same size as \code{tags}.  It contains the properties of tags (\code{NA} if the tag is missing) and is named by the tag names.
#'
#' @details This function finds the first appearance of each tag and returns the property (contents within the bracket).  It suits for finding propeties unique to a game (e.g. meta information, date, players, rule set), but not for extracting tags appearing for multiple times, such as moves.
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
