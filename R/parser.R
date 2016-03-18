### tag parsers ###

#' Find the tag properties in sgf text.
#'
#' @param sgf   character vector of texts in sgf format
#' @param tags  character vector of tags
#' @param return_list logical that specifies the output format
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
#'   (e.g. player names or rule set),
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

  # Here, llply is used across tags rather than sgf.
  # This is probably more efficient if the number of tags are
  # not likely to grow much, while the length of sgf may be long.
  DF <- sprintf("(?<![A-Z])%s\\[(.*?)(?<!\\\\)\\]", tags) %>%
    lapply(function(p) stringr::str_match(sgf, p)[, 2]) %>%
    stats::setNames(tags) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if (return_list) {
    lapply(seq(nrow(DF)), function(i)
      as.character(DF[i,]) %>% stats::setNames(tags)) %>%
      unname()
  } else {
    DF
  }
}




#' Obtain setup and plays in sgf text.
#'
#' @param sgf  character vector of text in sgf format
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
  # plyr::ldply was not very fast
  tmp <- lapply(tmp, as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()

  # tags : character vector
  # props: list of 2-col matrix with unknown nrow
  # tags and props are of the same size
  tags <- tmp[[2]]
  props <- stringr::str_match_all(
    tmp[[3]], stringr::regex("\\[(.*?)\\]"))

  # expand index and tags as many as nrow of props
  # vectorize the 2nd columns of props
  len <- vapply(props, nrow, numeric(1))
  index <- Map(rep, index, len) %>% unlist()
  tags  <- Map(rep, tags, len) %>% unlist() %>% unname()
  props <- lapply(props, `[`, TRUE, 2) %>% unlist()

  # make a master dataframe
  # TODO: enhance --- seems Map ~ bind_rows is slow.
#   DF <- Map(function(tag, prop, id)
#     data.frame(id = id, tag = tag, prop = prop[, 2], stringsAsFactors = FALSE),
#     tags, props, index) %>%
#     dplyr::bind_rows() %>%
  DF <- data.frame(id = index, tag = tags, prop = props,
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(colour = 1L + (tag=="W"),
                  isMove = as.integer(tag %in% c("B", "W"))) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(move = cumsum(isMove),
                  x = substring(prop, 1, 1) %>% paste0(collapse = "") %>%
                    utf8ToInt() %>% `-`(96L),
                  y = substring(prop, 2, 2) %>% paste0(collapse = "") %>%
                    utf8ToInt() %>% `-`(96L)) %>%
    dplyr::select(move, x, y, colour, isMove) %>%
    dplyr::ungroup()
  if (return_list) {
    plyr::dlply(DF, "id", dplyr::select, -id)
  } else {
    DF
  }
}

