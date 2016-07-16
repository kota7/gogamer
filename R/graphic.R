
#' Set graphic parameters for go board
#' @details this function set the graphic parameters of goban image
#' uses default (.default_graph_param) in principle
#' replace the value specified in '...'
#' default values are not modified
#' returns a list of graphic parameters
#' @param ... Graphic paramters
#' @return list of graphic parameters
set_graphic_param <- function(...)
{
  out <- as.list(.default_graphic_param)
  args <- list(...)
  tochange <- intersect(names(out), names(args))
  out[tochange] <- args[tochange]

  # adjust size based on the boardsize
  if (out$adjustsize && is.numeric(out$boardsize)) {
    sizevars <- setdiff(names(out), "boardsize")
    sizevars <- sizevars[grep("size$", sizevars)]
    for (v in sizevars) out[[v]] <- out[[v]] / out$boardsize * 19
  }

  # adjust size based on the target width
  # here is magic formula for the size adjustment
  ratio <- 0.25 * (out$targetwidth - 4.8) + 1
  sizevars <- setdiff(names(out), "boardsize")
  sizevars <- sizevars[grep("size$", sizevars)]
  for (v in sizevars) out[[v]] <- out[[v]] * ratio

  return(out)
}
