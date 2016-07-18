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




#' Obtain setup and plays in sgf
#'
#' @param sgf  scalar character of text in sgf format
#'
#' @return  a \code{data.frame} object with four columns:
#'   \describe{
#'     \item{color}{color (1: black, 2: white)}
#'     \item{x}{x coordinate}
#'     \item{y}{y coordinate}
#'     \item{ismove}{TRUE: move, FALSE: setup}
#'   }
#' @export
#' @examples
#' get_moves("AB[pd][dp][pp][dd]W[dg]")
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
  tags <- Map(rep, tags, len) %>% unlist() %>% unname()
  props <- unlist(props)

  # define output
  out <- data.frame(
    color = ifelse(regexpr("W", tags) > 0, WHITE, BLACK),
    x = (substring(props, 1, 1) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    y = (substring(props, 2, 2) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    ismove = (tags %in% c("B", "W"))
  )

  return(out)
}


#' Obtains the point markers in sgf
#' @param sgf Scalar character of sgf text
#' @return \code{data.frame} with variables \code{x}, \code{y}, and \code{color}
#' @export
get_points <- function(sgf)
{
  tmp <- stringr::str_match_all(
    sgf, "(?<![A-Z])(TB|TW)((\\[[A-Za-z]{2}\\])+)")[[1]]

  if (nrow(tmp) == 0L)
    return(data.frame(color = integer(0), x = integer(0), y = integer(0)))

  if (nrow(tmp) > 2L) {
    warning("TW, TB tags shouls appear only once in a game; ",
            "only the last appearance is used")
    index <- c(grep("TW", tmp[, 2]) %>% utils::tail(1),
               grep("TB", tmp[, 2]) %>% utils::tail(1))
    tmp <- tmp[index,]
  }

  tags <- tmp[, 2]
  # props: list of character vectors
  #        this is a vector since a tag may be associated with
  #        multiple positions, e.g. AB[pp][dd]
  props <- tolower(tmp[, 3]) %>% stringr::str_extract_all("[a-z]{2}")
  len <- lapply(props, length) %>% unlist()
  # expand tags
  tags <- Map(rep, tags, len) %>% unlist() %>% unname()
  props <- unlist(props)

  # define output
  out <- data.frame(
    color = ifelse(tags == "TW", WHITE, BLACK),
    x = (substring(props, 1, 1) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L)),
    y = (substring(props, 2, 2) %>% paste(collapse = "") %>%
           utf8ToInt() %>% `-`(96L))
  )
  return(out)
}