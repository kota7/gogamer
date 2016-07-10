
#' Draw go board
#' @param boardsize integer of boardsize
#' @param ... graphic parameters
#' @return \code{ggplot} object of goban
#' @examples
#' ggoban(19)
#' @export
ggoban <- function(boardsize, ...)
{
  # TODO:
  #   for now, board margin is fixed to 1
  #   but may be better to have it adjusted by the boardsize

  # dummy data for board grid
  dat <- dplyr::bind_rows(
    data.frame(x = 1, y = 1:boardsize, xend = boardsize, yend = 1:boardsize),
    data.frame(x = 1:boardsize, y = 1, xend = 1:boardsize, yend = boardsize)
  )

  # set local graphic paramters
  # i.e. if a paramter is given in '...',
  #      use it
  #      otherwise use the default setting
  graphic_param <- set_graphic_param(...)

  # trancate x and y labels
  graphic_param$xlabels <- graphic_param$xlabels[1:boardsize]
  graphic_param$ylabels <- graphic_param$ylabels[1:boardsize]


  out <- ggplot2::ggplot(dat) +
    # fix aspect ratio
    ggplot2::coord_fixed() +
    # set the board size
    ggplot2::scale_x_continuous(
      breaks = 1:boardsize, limits = c(0, boardsize + 1)) +
    ggplot2::scale_y_continuous(
      breaks = 1:boardsize, limits = c(0, boardsize + 1)) +
    # make the board empty and fill by specified color
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = graphic_param$boardcolor)
    ) +
    # draw grid
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color = graphic_param$gridcolor)


  # dummy data for board grid
  dat <- dplyr::bind_rows(
    data.frame(x = 1:boardsize, y = 0,
               label = graphic_param$xlabels, stringsAsFactors = FALSE),
    data.frame(x = 1:boardsize, y = boardsize+1,
               label = graphic_param$xlabels, stringsAsFactors = FALSE),
    data.frame(x = 0, y = 1:boardsize,
               label = graphic_param$ylabels, stringsAsFactors = FALSE),
    data.frame(x = boardsize+1, y = 1:boardsize,
               label = graphic_param$ylabels, stringsAsFactors = FALSE)
  )

  # add labels at four edges
  # decided not to use axis labels since ggplot2 does not support
  # editing axis label positions, and this won't change soon
  # cowplot::switch_axis_position is an option but seems not very stable
  # rather, axis labels are added manually using geom_text()
  out <- out +
    ggplot2::geom_text(
      data = dat, ggplot2::aes(x = x, y = y, label = label),
      size = graphic_param$labelsize, color = graphic_param$labelcolor)


  # add stars
  dat <- star_position(boardsize)
  out <- out +
    ggplot2::geom_point(
      data = dat, ggplot2::aes(x = x, y = y),
      size = graphic_param$starsize, color = graphic_param$starcolor)

  # give class name and boardsize attribute
  class(out) <- c("ggoban", class(out))
  attr(out, "boardsize") <- boardsize
  return(out)
}


#' @export
print.ggoban <- function(x, quiet = FALSE, ...)
{
  NextMethod(x)
  if (!quiet) {
    cat("will suggest saving size...\n")
  }
}
