### tag parsers ###

#' Find the tag properties in sgf text.
#'
#' @param sgf   character vector of texts in sgf format
#' @param tags  character vector of tags
#' @param return_list logical that specifies
#'
#' @return  If \code{return_list} is TRUE, this function returns
#'   a list with the same length as \code{sgf}.  Each element of the list
#'   is a character vector of tag properties named by tags.
#'   If \code{return_list} is FALSE, then the function returns
#'   a \code{data.frame} object, where rows correspond to sgf input,
#'   and columns correspond to tags.
#'
#' @details This function finds the first appearance of each tag
#'   and returns the property (contents within the bracket).
#'   It suits for finding propeties unique to a game
#'   (e.g. meta information, date, players, rule set),
#'   but not for extracting tags appearing for multiple times, such as moves.
#'
#' @examples
#' # with a length-1 sgf
#' sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
#' get_props(sgf, "PB")
#' get_props(sgf, c("PW", "PB", "EV", "RO"))
#'
#' # with a vector sgf
#' sgf <- c("(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])",
#'          "(;GM[1]PW[Ichiriki]PB[Ida]RO[Final]EV[NHK])")
#' get_props(sgf, c("PB", "PW"), FALSE)
#' get_props(sgf, c("PB", "PW"), TRUE)
#'
#' @export
get_props <- function(sgf, tags, return_list = TRUE) {
  DF <- sprintf("(?<![A-Z])%s\\[(.*?)(?<!\\\\)\\]", tags) %>%
    plyr::llply(function(p)
      stringr::str_match(sgf, p)[, 2]) %>% stats::setNames(tags) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if (return_list) {
    plyr::alply(DF, 1, function(d) as.character(d) %>% setNames(tags)) %>%
      stats::setNames(NULL)
  } else {
    DF
  }
}



#' Obtain setup and plays in sgf text.
#'
#' @param sgf  a character of text in sgf format
#' @param return_list  logical that specifies output form.
#'
#' @return  The ouput for each sgf is a \code{tbl_df} object with five columns:
#'   \itemize{
#'     \item{move: move number}
#'     \item{x: x coordinate}
#'     \item{y: y coordinate}
#'     \item{colour: colour (1: black, 2: white)}
#'     \item{isMove: 1: move, 0: setup}
#'   }
#'   If \code{return_list} is TRUE, this function returns a list of such objects.
#'   Otherwise, it returns a big \code{tbl_df} object with an additional column of \code{id},
#'   which indicates which input the row corresponds to.
#' @export
get_moves <- function(sgf, return_list = TRUE) {
  tmp <- stringr::str_match_all(sgf, "(?<![A-Z])(AB|AW|B|W)((\\[.*?\\])+)")

  # All tag and props are collected in a long matrix and parse simulatanuously
  # index tracks which input each row corresponds to
  len <- vapply(tmp, nrow, integer(1))
  index <- Map(function(l, i) rep(i, l), len, seq_along(len)) %>% unlist()

  # big data.frame of all
  tmp <- plyr::ldply(tmp, as.data.frame, stringsAsFactors = FALSE)

  tags <- tmp[, 2]
  props <- stringr::str_match_all(
    tmp[, 3], stringr::regex("\\[(.*?)\\]", dotall = TRUE))

  # make a master dataframe
  DF <- Map(function(tag, prop, id)
    data.frame(id = id, tag = tag, prop = prop[, 2], stringsAsFactors = FALSE),
    tags, props, index) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(colour = 1L + (tag=="W"),
                  isMove = as.integer(tag %in% c("B", "W"))) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(move = cumsum(isMove),
                  x = substring(prop, 1, 1) %>% paste0(collapse = "") %>%
                    utf8ToInt() %>% `-`(96L),
                  y = substring(prop, 2, 2) %>% paste0(collapse = "") %>%
                    utf8ToInt() %>% `-`(96L)) %>%
    dplyr::select(move, x, y, colour, isMove)
  if (return_list) {
    plyr::dlply(DF, "id", dplyr::select, -id)
  } else {
    DF
  }
}

