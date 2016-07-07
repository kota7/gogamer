
#' ggplot go board
#' @param boardsize integer of boardsize
#' @param gridcolor color for the grid
#' @param boardcolor color for the background
#' @param labelcolor color for the label text
#' @param labelsize numeric of the size of label
#' @param xlabels,ylabels character vector of the labels for each axis
#' @return \code{ggplot} object of goban
#' @examples
#'   ggoboard(19)
#' @export
ggoboard <- function(boardsize,
                     gridcolor = "#262626", boardcolor = "#e1f0c0",
                     labelcolor = gridcolor, labelsize = 3.5,
                     xlabels = LETTERS[-9], ylabels = as.character(1:25))
{
  # TODO:
  #   for now, board margin is fixed to 1
  #   but may be better to have it adjusted by the boardsize

  # dummy data for board grid
  dat <- dplyr::bind_rows(
    data.frame(x = 1, y = 1:boardsize, xend = boardsize, yend = 1:boardsize),
    data.frame(x = 1:boardsize, y = 1, xend = 1:boardsize, yend = boardsize)
  )

  # trancate x and y labels
  xlabels <- xlabels[1:boardsize]
  ylabels <- ylabels[1:boardsize]

  out <- ggplot2::ggplot(dat) +
    # fix aspect ratio
    ggplot2::coord_fixed() +
    # set the board size
    ggplot2::scale_x_continuous(
      breaks = 1:boardsize, limits = c(0, boardsize + 1), name = "") +
    ggplot2::scale_y_continuous(
      breaks = 1:boardsize, limits = c(0, boardsize + 1), name = "") +
    # make the board empty and fill by specified color
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = boardcolor)
    ) +
    # draw grid
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color = gridcolor)


  # dummy data for board grid
  dat <- dplyr::bind_rows(
    data.frame(x = 1:boardsize, y = 0, label = xlabels,
               stringsAsFactors = FALSE),
    data.frame(x = 1:boardsize, y = boardsize+1, label = xlabels,
               stringsAsFactors = FALSE),
    data.frame(x = 0, y = 1:boardsize, label = ylabels,
               stringsAsFactors = FALSE),
    data.frame(x = boardsize+1, y = 1:boardsize, label = ylabels,
               stringsAsFactors = FALSE)
  )

  # add labels at four edges
  # decided not to use axis labels since ggplot2 does not support
  # editing axis label positions, and this won't change soon
  # cowplot::switch_axis_position is an option but seems not very stable
  # rather, axis labels are added manually using geom_text()
  out <- out +
    ggplot2::geom_text(data = dat,
                       ggplot2::aes(x = x, y = y, label = label),
                       size = labelsize, color = labelcolor)

  return(out)
}

