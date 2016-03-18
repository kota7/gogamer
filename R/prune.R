#' Remove brances of SGF
#' @param sgf  Scalar character formatted as SGF
#' @param keep_first  Logical. If TRUE, keep the first branch.
#'   Otherwise, keep the last branch.
#' @return Scalar character of SGF where branches are removed
#' @export
prune_sgf <- function(sgf, keep_first = FALSE)
{
  PruneSgf(sgf, keep_first)
}
