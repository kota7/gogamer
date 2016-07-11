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

    # numbers
    whitenumbercolor = "#000000", blacknumbercolor = "#ffffff",
    numbersize = 3,

    # marker
    whitemarkercolor = "#000000", blackmarkercolor = "#ffffff",
    markersize = 3.5,
    emptymarkercolor = "#262626",  # empty markers are markers not on stone
    emptyshadowsize = 5,           # size of shadow around empty markers

    # board
    boardcolor = "#e1f0c0", gridcolor  = "#262626",
    labelcolor = "#262626", labelsize = 3.5,
    starcolor = "#262626", starsize = 1.5,
    # axis labels
    xlabels = LETTERS[-9], ylabels = as.character(1:25),

    # kifu outside notes
    notebackcolor = "#d2f1f1", notetextcolor = "#262626",
    notestonesize = 3.5, notenumbersize = 2.5,
    notetextsize = 3,
    moveperrow = 8,  # number of moves to show per line in the outside note

    # symbols to use for printing on console
    black_mark = "@", white_mark = "O", empty_mark = "+"
  ),
  parent = emptyenv()
)








