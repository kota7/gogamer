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
get_moves <- function(sgf) {
  tmp <- stringr::str_match_all(
    sgf, "(?<![A-Z])(AB|AW|B|W)((\\[[A-Za-z]{2}\\])+)")[[1]]

  # tags: character vector of tags
  tags <- tmp[, 2]
  # props: list of character vectors
  #        this is a vector since a tag may be associated with
  #        multiple positions, e.g. AB[pp][dd]
  props <- tolower(tmp[, 3]) %>% stringr::str_extract_all("[a-z]{2}")
  len <- lapply(props, length) %>% unlist()
  # expand tags
  tags <- Map(rep, tags, len) %>% unlist()
  props <- unlist(props)

  # define output
  out <- data.frame(
    color  = 1,
    x = (substring(props, 1, 1) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    y = (substring(props, 2, 2) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    ismove = (tags %in% c("B", "W"))
  ) %>% dplyr::mutate(color = replace(color, grep("W", tags), 2))

  return(out)
}

