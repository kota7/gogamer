
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


  # set endogenous parameters
  # boardlimit
  if (out$axislabels) {
    # add 1 to each side for labels
    # seems this works well for most boardsize
    boardlimits <- c(0, out$boardsize + 1)
  } else {
    # if the axis are not added, the margin size should be taken into account
    marginsize <- 0.03 * (19 - out$boardsize)
    boardlimits <- c(1 - marginsize, out$boardsize + marginsize)
  }
  out$endogenous$boardlimits <- boardlimits


  # size parameters
  # get size parameter names
  sizevars <- setdiff(names(out), "boardsize")
  sizevars <- sizevars[grep("size$", sizevars)]
  gobansizevars <- sizevars[regexpr("^note", sizevars) < 0]
  notesizevars <- sizevars[regexpr("^note", sizevars) > 0]

  # initialize size parameters
  out$endogenous[sizevars] <- out[sizevars]

  # adjust size based on the effective boardsize
  effective_boardsize <- diff(boardlimits)
  ratio <- (effective_boardsize - 20)/19 + 1
  if (out$adjustsizeonboard && is.numeric(out$boardsize)) {
    for (v in gobansizevars)
      #out$endogenous[[v]] <- out$endogenous[[v]] / effective_boardsize * 20
      out$endogenous[[v]] <- out$endogenous[[v]] / ratio
  }
  if (out$adjustsizeonnote && is.numeric(out$boardsize)) {
    for (v in notesizevars)
      out$endogenous[[v]] <- out$endogenous[[v]] / out$boardsize * 18
  }

  # adjust size based on the target width
  # here is magic formula for the size adjustment
  ratio <- 0.25 * (out$targetwidth - 4.8) + 1
  for (v in sizevars)
    out$endogenous[[v]] <- out$endogenous[[v]] * ratio


  return(out)
}


#' Update graphic parameters
#' @param params A list of graphic parameters
#' @param ... Graphic parameters
#' @return An updated list of graphic parameters
update_graphic_param <- function(params, ...)
{
  newparams <- list(...)
  toappend <- setdiff(names(params), names(newparams))
  args <- c(newparams, params[toappend])
  out <- do.call(set_graphic_param, args = args)
  return(out)
}
