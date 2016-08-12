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



#' Parse SGF of each node
#' @param sgf A character vector of SGF nodes
#'
#' @return A list
parse_sgfnode <- function(sgf)
{
  n <- length(sgf)

  # parse moves. By SGF rule, there must be only one move in each node
  tmp <- stringr::str_match(
    sgf, "(?<![A-Z])(B|W)\\[([A-Za-z]{0,2})\\]")
  # compute coordinates
  x1 <- rep(NA_integer_, n)
  x2 <- rep(NA_integer_, n)
  # empty = (0, 0)
  x1[which(tmp[, 3] == "")] <- 0
  x2[which(tmp[, 3] == "")] <- 0

  index <- grep("[a-z]+", tmp[, 3])
  x1[index] <- substring(tmp[index, 3], 1, 1) %>%
    paste0(collapse = "") %>% utf8ToInt() %>% `-`(96L)
  x2[index] <- substring(tmp[index, 3], 2, 2) %>%
    paste0(collapse = "") %>% utf8ToInt() %>% `-`(96L)
  index <- which(!is.na(tmp[, 1]))
  out1 <- data.frame(
    id = index, color = ifelse(tmp[index, 2] == "W", WHITE, BLACK),
    x = x1[index], y = x2[index], ismove = TRUE)
  ####

  # parse 'add black' and 'add white'
  # and 'points'
  tmp <- stringr::str_match(
    sgf, "(?<![A-Z])A(B)((\\[(.*?)\\])+)") %>%
    rbind(stringr::str_match(
      sgf, "(?<![A-Z])A(W)((\\[(.*?)\\])+)")) %>%
    rbind(stringr::str_match(
      sgf, "(?<![A-Z])T(B)((\\[(.*?)\\])+)")) %>%
    rbind(stringr::str_match(
      sgf, "(?<![A-Z])T(W)((\\[(.*?)\\])+)"))

  id <- which(!is.na(tmp[, 1]))
  cl <- tmp[id, 2]
  ps <- tmp[id, 3]
  tp <- c("setup", "territory")[as.integer((id-1)/(2*n)) + 1]
  id <- ((id - 1L) %% n) + 1L

  # expand ps: e.g. [ab][cd]...
  ps <- stringr::str_extract_all(ps, "[^\\[\\]]+")
  len <- sapply(ps, length)

  # expand color, id, type
  cl <- Map(rep, cl, len) %>% unlist() %>% unname()
  id <- Map(rep, id, len) %>% unlist() %>% unname()
  tp <- Map(rep, tp, len) %>% unlist() %>% unname()
  ps <- unlist(ps)

  # expand props with colon, e.g. bb:cd
  withColon <- (substring(ps, 3, 3) == ":")
  x1 <- substring(ps, 1, 1)
  y1 <- substring(ps, 2, 2)
  x2 <- substring(ps, 4, 4)
  y2 <- substring(ps, 5, 5)
  # impute the same letter if there is no ':'
  x2[!withColon] <- x1[!withColon]
  y2[!withColon] <- y1[!withColon]

  x <- Map(seq,
           utf8ToInt(paste0(x1, collapse = "")),
           utf8ToInt(paste0(x2, collapse = ""))) %>%
    lapply(`-`, 96L)
  y <- Map(seq,
           utf8ToInt(paste0(y1, collapse = "")),
           utf8ToInt(paste0(y2, collapse = ""))) %>%
    lapply(`-`, 96L)
  d <- Map(expand.grid, x = x, y = y)
  # expand color, id, type
  len <- lapply(d, nrow) %>% unlist()
  cl <- Map(rep, cl, len) %>% unlist() %>% unname()
  id <- Map(rep, id, len) %>% unlist() %>% unname()
  tp <- Map(rep, tp, len) %>% unlist() %>% unname()

  out2 <- data.frame(id = id, color = ifelse(cl == "W", WHITE, BLACK)) %>%
    dplyr::bind_cols(dplyr::bind_rows(d)) %>%
    dplyr::mutate(type = tp)
  ####

  # parse comments
  tmp <- stringr::str_match(
    sgf, "(?<![A-Z])(C)\\[([^\\[\\]]*?)\\]")
  index <- which(!is.na(tmp[, 1]))
  comments <- data.frame(
    id = index, comment = tmp[index, 3], stringsAsFactors = FALSE)
  ####

  # finalizing outputs
  # moves and setups
  out <- out2 %>%
    dplyr::filter_(~type == "setup") %>% dplyr::select_(~ -type) %>%
    dplyr::mutate(ismove = FALSE) %>%
    dplyr::bind_rows(out1) %>%
    dplyr::arrange_(~id, ~dplyr::desc(ismove))
  out <- lapply(seq(n), function(i)
    out %>% dplyr::filter_(~id %in% i) %>% dplyr::select_(~-id))
  # points
  points <- out2 %>%
    dplyr::filter_(~type == "territory") %>% dplyr::select_(~ -type)
  points <- lapply(seq(n), function(i)
    points %>% dplyr::filter_(~id %in% i) %>% dplyr::select_(~-id))
  # comments
  comments <- lapply(seq(n), function(i)
    comments %>% dplyr::filter_(~id %in% i) %>% `[[`("comment"))

  return(list(moves = out, points = points, comments = comments))
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
    sgf, "(?<![A-Z])(AB|AW|B|W)((\\[[A-Za-z]{2}(:[A-Za-z]{2}){0,1}\\])+)")[[1]]

  # tags: character vector of tags
  tags <- tmp[, 2]
  # props: list of character vectors
  #        this is a vector since a tag may be associated with
  #        multiple positions, e.g. AB[pp][dd]
  props <- tolower(tmp[, 3]) %>% stringr::str_extract_all("[a-z:]{2,5}")
  len <- lapply(props, length) %>% unlist()
  # expand tags
  tags <- Map(rep, tags, len) %>% unlist() %>% unname()
  props <- unlist(props)

  # expand props with colon, e.g. bb:cd
  withColon <- (substring(props, 3, 3) == ":")
  x1 <- substring(props, 1, 1)
  y1 <- substring(props, 2, 2)
  x2 <- substring(props, 4, 4)
  y2 <- substring(props, 5, 5)
  # impute the same letter if there is no ':'
  x2[!withColon] <- x1[!withColon]
  y2[!withColon] <- y1[!withColon]

  x <- Map(seq,
           utf8ToInt(paste0(x1, collapse = "")),
           utf8ToInt(paste0(x2, collapse = ""))) %>%
    lapply(`-`, 96L)
  y <- Map(seq,
           utf8ToInt(paste0(y1, collapse = "")),
           utf8ToInt(paste0(y2, collapse = ""))) %>%
    lapply(`-`, 96L)
  d <- Map(expand.grid, x = x, y = y)
  # tags must be repeated as many as the number of rows
  len <- lapply(d, nrow) %>% unlist()
  tags <- Map(rep, tags, len) %>% unlist() %>% unname()

  out <- data.frame(color = ifelse(regexpr("W", tags) > 0, WHITE, BLACK)) %>%
    dplyr::bind_cols(dplyr::bind_rows(d)) %>%
    dplyr::mutate(ismove = tags %in% c("B", "W"))

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
    warning("TW, TB tags should appear only once in a game; ",
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
