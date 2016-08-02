
#' Draw go board
#' @param boardsize integer of boardsize
#' @param ... graphic parameters
#' @return Object of class \code{ggoban}, which inherits \code{ggplot}
#' @examples
#' ggoban(19)
#' @export
ggoban <- function(boardsize, ...)
{
  # set local graphic paramters
  # i.e. if a paramter is given in '...',
  #      use it
  #      otherwise use the default setting
  graphic_param <- set_graphic_param(boardsize = boardsize, ...)

  # trancate x and y labels
  graphic_param$xlabels <- graphic_param$xlabels[1:boardsize]
  graphic_param$ylabels <- graphic_param$ylabels[1:boardsize]

  # board limits,
  #if (graphic_param$axislabels) {
  #  # add 1 to each side for labels
  #  boardlimits <- c(0, boardsize + 1)
  #} else {
  #  # if the axis are not added, the margin size should be taken into account
  #  marginsize <- 0.05 * (boardsize - 19)
  #  boardlimits <- c(1 - marginsize, boardsize + marginsize)
  #}
  boardxlim <- graphic_param$endogenous$boardxlim
  boardylim <- graphic_param$endogenous$boardylim

  # dummy data for board grid
  dat <- dplyr::bind_rows(
    data.frame(x = 1L, y = 1L:boardsize, xend = boardsize, yend = 1L:boardsize),
    data.frame(x = 1L:boardsize, y = 1L, xend = 1L:boardsize, yend = boardsize)
  )
  # remove cases out of bounds
  oob <- ((dat$x == dat$xend) & (dat$x < boardxlim[1] | dat$x > boardxlim[2])) |
    ((dat$y == dat$yend) & (dat$y < boardylim[1] | dat$y > boardylim[2]))
  dat <- dat[!oob,]
  # trancate x and y coorinates by the boardlimits
  dat$x <- pmax(dat$x, boardxlim[1]) %>% pmin(boardxlim[2])
  dat$xend <- pmax(dat$xend, boardxlim[1]) %>% pmin(boardxlim[2])
  dat$y <- pmax(dat$y, boardylim[1]) %>% pmin(boardylim[2])
  dat$yend <- pmax(dat$yend, boardylim[1]) %>% pmin(boardylim[2])
  dat <- dat[!duplicated(dat),]
  #print(dat)

  out <- ggplot2::ggplot(dat) +
    # fix aspect ratio
    ggplot2::coord_fixed() +
    # set the board size
    ggplot2::scale_x_continuous(
      breaks = 1:boardsize, limits = boardxlim) +
    ggplot2::scale_y_continuous(
      breaks = 1:boardsize, limits = boardylim) +
    # make the board empty and fill by specified color
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = scales::alpha(graphic_param$boardcolor,
                             graphic_param$boardalpha))
    ) +
    # draw grid
    ggplot2::geom_segment(ggplot2::aes_string(x = "x", y = "y",
                                              xend = "xend", yend = "yend"),
                          color = graphic_param$gridcolor)


  if (graphic_param$axislabels) {
    # dummy data for board axis labels
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
    # filter the points out of bounds
    dat <- dat %>% dplyr::filter_(~x >= boardxlim[1], ~x <= boardxlim[2],
                                  ~y >= boardylim[1], ~y <= boardylim[2])

    # add labels at four edges
    # decided not to use axis labels since ggplot2 does not support
    # editing axis label positions, and this won't change soon
    # cowplot::switch_axis_position is an option but seems not very stable
    # rather, axis labels are added manually using geom_text()
    out <- out +
      ggplot2::geom_text(
        data = dat, ggplot2::aes_string(x = "x", y = "y", label = "label"),
        size = graphic_param$endogenous$axislabelsize,
        color = graphic_param$axislabelcolor)
  }

  # add stars
  dat <- star_position(boardsize)
  dat <- dat %>% dplyr::filter_(~x >= boardxlim[1], ~x <= boardxlim[2],
                                ~y >= boardylim[1], ~y <= boardylim[2])

  out <- out +
    ggplot2::geom_point(
      data = dat, ggplot2::aes_string(x = "x", y = "y"),
      size = graphic_param$endogenous$starsize, color = graphic_param$starcolor)

  # give class name and boardsize attribute
  class(out) <- c("ggoban", class(out))

  # store the graphic_param for the later reference
  attr(out, "graphic_param") <- graphic_param
  return(out)
}


#' @export
print.ggoban <- function(x, ...)
{
  NextMethod(x)  # this will call "print.ggplot"
}


#' Check if object is ggoban class
#' @param x R object
#' @return Logical. True if and only if \code{obj} inherits \code{ggoban} class.
#' @export
is.ggoban <- function(x)
{
  return(inherits(x, "ggoban"))
}
