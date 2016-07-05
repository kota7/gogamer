### In this file, S3 class 'gogame' is defined ###


print.gogame <- function(x, ...)
{
  cat("\n*** Go game ***\n")
  cat(sprintf(" %s (W) vs %s\n", x[["PW"]], x[["PB"]]))
  cat(sprintf(" %s (%d moves)\n", x[["RE"]], nrow(x[["transition"]])))
  cat("***\n")
}



#' Return the board state
#'
stateat <- function(x, at)
{
  densmat <- x[["transition"]] %>%
    dplyr::filter(move <= at) %>%
    dplyr::group_by(x, y) %>% dplyr::summarize(value = sum(value)) %>%
    dplyr::filter(value > 0L)
}


#' Plot the board
#'
plotat <- function(x, at)
{ }

#' Generate kifu (game record) document
kifu <- function(x)
{
}