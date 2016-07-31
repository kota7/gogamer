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
    blackcolor = "#111111", whitecolor = "#f5f5f5",
    stonelinecolor = "#101010", stonealpha = 1,
    stonesize = 6, stonelinewidth = 0.7,

    # numbers
    whitenumbercolor = "#0f0f0f", blacknumbercolor = "#f0f0f0",
    numbersize = 3,

    # label
    whitelabelcolor = "#0f0f0f",
    blacklabelcolor = "#f0f0f0",
    emptylabelcolor = "#262626",
    emptylabelshadowsize = 5,       # size of shadow around empty markers
    labelsize = 3.5,

    # marker
    whitemarkercolor = "#0f0f0f",
    blackmarkercolor = "#f0f0f0",
    emptymarkercolor = "#262626",  # empty markers are markers not on stone
    markersize = 2.5,

    lastmovemarker = 3,  # default maker used for last move indicator

    # score points
    territorysize = 2,
    territoryshape = 21,
    territorylinecolor = "#262626",
    territorylinewidth = 0.5,

    # board
    boardcolor = "#e7e18f", gridcolor  = "#262626",
    boardalpha = 1,
    axislabelcolor = "#262626", axislabelsize = 3.5,
    starcolor = "#262626", starsize = 1.5,
    # axis labels
    xlabels = LETTERS[-9], ylabels = as.character(1:25),
    axislabels = TRUE,  # if true, show axis labels

    # kifu outside notes
    notebackcolor = "#ffe5cc", notetextcolor = "#101010",
    notestonesize = 5, notenumbersize = 2.5,
    notetextsize = 3,
    moveperrow = 8,  # number of moves to show per line in the outside note
    adjustorigin = TRUE,
    # if this is true, the numebers in kifu are deducted by a multiple of 100
    # whenever appropriate

    # symbols to use for printing on console
    blackmark = "@", whitemark = "O", emptymark = "+",


    # color theme
    # if specified, all color parameters are changed
    # currently supports:
    # - standard (default)
    # - bw
    # - pastel
    colortheme = NULL,

    # parameters endogeneously determined
    # these are supposed to filled by a call of set_graphic_param()
    endogenous = list(
      # for ggoban object
      boardlimits = NULL,  # length of board edge
      stonesize = NULL,    # size of stones
      numbersize = NULL,   # size of numbers on stone
      labelsize = NULL,    # size of labels on stone
      emptylabelshadowsize = NULL, # size of label background put on empty point
      markersize = NULL,    # size of markers on stone or board
      territorysize = NULL, # size of territory points
      axislabelsize = NULL, # size of axis label
      starsize = NULL,      # size of star marker

      # for kifu outside note
      notestonesize = NULL,    # size of stones on note
      notenumbersize = NULL,   # size of numbers on note
      notetextsize = NULL     # size of text on note

    )
  ),
  parent = emptyenv()
)



# predefined color template
# referred to when 'colortheme' graphic parameter is specified
.color_themes <- list(

  # standard color set
  standard = list(
    boardcolor = "#e7e18f", starcolor = "#262626",
    boardalpha = 1,
    gridcolor = "#262626", axislabelcolor = "#262626",

    blackcolor = "#111111", whitecolor = "#f5f5f5",
    stonealpha = 1,
    stonelinecolor = "#101010",
    blacknumbercolor = "#f0f0f0", whitenumbercolor = "#0f0f0f",
    blacklabelcolor = "#f0f0f0", whitelabelcolor = "#0f0f0f",
    emptylabelcolor = "#262626",
    blackmarkercolor = "#f0f0f0", whitemarkercolor = "#0f0f0f",
    emptymarkercolor = "#262626",
    territorylinecolor = "#262626",

    notebackcolor = "#ffe5cc", notetextcolor = "#101010"
  ),

  bw = list(
    boardcolor = "#ffffff", starcolor = "#000000",
    boardalpha = 1,
    gridcolor = "#000000", axislabelcolor = "#000000",

    blackcolor = "#000000", whitecolor = "#ffffff",
    stonealpha = 1,
    stonelinecolor = "#000000",
    blacknumbercolor = "#ffffff", whitenumbercolor = "#000000",
    blacklabelcolor = "#ffffff", whitelabelcolor = "#000000",
    emptylabelcolor = "#000000",
    blackmarkercolor = "#ffffff", whitemarkercolor = "#000000",
    emptymarkercolor = "#000000",
    territorylinecolor = "#000000",

    notebackcolor = "#ffffff", notetextcolor = "#000000"
  ),

  pastel = list(
    boardcolor = "#f4ffea", starcolor = "#552055",
    boardalpha = 1,
    gridcolor = "#552055", axislabelcolor = "#552055",

    blackcolor = "#84c1ff", whitecolor = "#ffc184",
    stonealpha = 1,
    stonelinecolor = "#7f607f",
    blacknumbercolor = "#552055", whitenumbercolor = "#552055",
    blacklabelcolor = "#552055", whitelabelcolor = "#552055",
    emptylabelcolor = "#552055",
    blackmarkercolor = "#552055", whitemarkercolor = "#552055",
    emptymarkercolor = "#552055",
    territorylinecolor = "#552055",

    notebackcolor = "#f7ffef", notetextcolor = "#552055"
  ),

  dark = list(
    boardcolor = "#111111", starcolor = "#dedede",
    boardalpha = 1,
    gridcolor = "#dedede", axislabelcolor = "#dedede",

    blackcolor = "#050505", whitecolor = "#f9f9f9",
    stonealpha = 1,
    stonelinecolor = "#888888",
    blacknumbercolor = "#ffffff", whitenumbercolor = "#000000",
    blacklabelcolor = "#ffffff", whitelabelcolor = "#000000",
    emptylabelcolor = "#000000",
    blackmarkercolor = "#ffffff", whitemarkercolor = "#000000",
    emptymarkercolor = "#000000",
    territorylinecolor = "#888888",

    notebackcolor = "#222222", notetextcolor = "#dddddd"
  ),

  crystal = list(
    boardcolor = "#ffffef", starcolor = "#262626",
    boardalpha = 1,
    gridcolor = "#006047", axislabelcolor = "#006047",

    blackcolor = "#93c9ff", whitecolor = "#ff9393",
    stonealpha = 0.8,
    stonelinecolor = "#262626",
    blacknumbercolor = "#552055", whitenumbercolor = "#552055",
    blacklabelcolor = "#552055", whitelabelcolor = "#552055",
    emptylabelcolor = "#552055",
    blackmarkercolor = "#552055", whitemarkercolor = "#552055",
    emptymarkercolor = "#552055",
    territorylinecolor = "#552055",

    notebackcolor = "#fffff4", notetextcolor = "#552055"

  )

)



