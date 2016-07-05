### tag parsers ###

#' Find the tag properties in sgf text.
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
#' @examples
#' sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
#' get_props(sgf, "PB")
#' get_props(sgf, c("PW", "PB", "EV", "RO"))
#'
#' @export
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




#' Obtain setup and plays in sgf text.
#'
#' @param sgf  scalar character of text in sgf format
#'
#' @return  a \code{data.frame} object with five columns:
#'   \itemize{
#'     \item{color: color (1: black, 2: white)}
#'     \item{x: x coordinate}
#'     \item{y: y coordinate}
#'     \item{ismove: TRUE: move, FALSE: setup}
#'   }
#' @export
get_moves <- function(sgf) {
  tmp <- stringr::str_match_all(
    sgf, "(?<![A-Z])(AB|AW|B|W)((\\[[A-Za-z]{2}\\])+)")[[1]]

  # tags : character vector
  # props: list of 2-col matrix with unknown nrow
  # tags and props are of the same size
  tags <- tmp[, 2]
  props <- tolower(substring(tmp[, 3], 2, 3))

  # define output
  out <- data.frame(
    color  = 1,
    x = (substring(props, 1, 1) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    y = (substring(props, 2, 2) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    ismove = (nchar(tags) == 1)
  ) %>% dplyr::mutate(color = replace(color, grep("W", tags), 2))

  return(out)
}

