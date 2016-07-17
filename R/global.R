### this file defines parameters used globally within the package ###


# indicator for colors
# define these to avoid hardcoding
# they are unlikely to change, though
BLACK <- 1L
WHITE <- 2L



# default graphic setting
.default_graphic_param <- list2env(
  list(
    targetwidth = 5,
    # this specifies the output width used for
    # 'ggsave' or insertion to rmarkdown document
    # all sizes are proportionately adjusted
    # at the call of 'set_graphic_param'

    boardsize = 19,
    adjustsizeonboard = TRUE,
    # if this is true, all sizes on board are adjusted by boardsize
    # assuming the supplied numbers corresponds to board size 19
    adjustsizeonnote = FALSE,
    # the same for sizes on outside notes


    # stone
    blackcolor = "#111111", whitecolor = "#f5f5f5", stonelinecolor = "#101010",
    stonesize = 6, stonelinewidth = 0.7,

    # numbers
    whitenumbercolor = "#0f0f0f", blacknumbercolor = "#f0f0f0",
    numbersize = 3,

    # label
    whitelabelcolor = "#0e0e0e",
    blacklabelcolor = "#f0f0f0",
    emptylabelcolor = "#262626",
    emptylabelshadowsize = 5,       # size of shadow around empty markers
    labelsize = 3.5,

    # marker
    whitemarkercolor = "#0e0e0e",
    blackmarkercolor = "#f0f0f0",
    emptymarkercolor = "#262626",  # empty markers are markers not on stone
    markersize = 2.5,

    lastmovemarker = 3,  # default maker used for last move indicator

    # score points
    territorysize = 2,
    territoryshape = 21,
    territorylinecolor = "#262626",
    territoryslinewidth = 0.5,

    # board
    boardcolor = "#e2f1c1", gridcolor  = "#262626",
    axislabelcolor = "#262626", axislabelsize = 3.5,
    starcolor = "#262626", starsize = 1.5,
    # axis labels
    xlabels = LETTERS[-9], ylabels = as.character(1:25),

    # kifu outside notes
    notebackcolor = "#eff7df", notetextcolor = "#101010",
    notestonesize = 5, notenumbersize = 2.5,
    notetextsize = 3,
    moveperrow = 8,  # number of moves to show per line in the outside note
    adjustorigin = TRUE,
    # if this is true, the numebers in kifu are deducted by a multiple of 100
    # whenever appropriate

    # symbols to use for printing on console
    blackmark = "@", whitemark = "O", emptymark = "+"
  ),
  parent = emptyenv()
)








