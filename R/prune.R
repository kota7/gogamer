#' Remove brances of SGF
#' @param sgf  Scalar character formatted as SGF
#' @return Scalar character of SGF where branches are removed
#' @export
prune_sgf <- function(sgf)
{
  PruneSgf(sgf)
}
