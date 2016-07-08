### this file defines parameters used globally within the package ###


# indicator for colors
# define these to avoid hardcoding
# they are unlikely to change, though
BLACK <- 1L
WHITE <- 2L



# default graphic setting
.default_graphic_param <- list2env(
  list(
    # stone
    blackcolor = "#000000", whitecolor = "#ffffff", stonelinecolor = "#000000",
    stonesize = 6,

    # marker
    whitemarkercolor = "#000000", blackmarkercolor = "#ffffff",
    markersize = 3,

    # board
    boardcolor = "#e1f0c0", gridcolor  = "#262626", labelcolor = "#262626",
    labelsize = 3.5,

    # axis labels
    xlabels = LETTERS[-9], ylabels = as.character(1:25)
  ),
  parent = emptyenv()
)








