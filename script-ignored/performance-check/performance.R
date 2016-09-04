library(microbenchmark)
library(lineprof)
library(gogamer)


kjd_sgf <- readLines("http://waterfire.us/Kogo's%20Joseki%20Dictionary.sgf") %>%
  paste0(collapse = "\n")
l <- lineprof(parse_sgf(kjd_sgf))
print(l)
#shine(l)

big_sgf <- readLines(system.file("testdata/move1e5.sgf.gz", package = "gogamer")) %>%
  paste0(collapse = "\n")
l <- lineprof(parse_sgf(big_sgf))
print(l)
#shine(l)

kjd <- parse_sgf(kjd_sgf)
movedf <- dplyr::filter(kjd$gametree$transition, ismove) %>% dplyr::rename(color = value)
microbenchmark(
  gogamer:::get_transition_wrapper(movedf, kjd$boardsize, kjd$gametree$children),
  gogamer:::check_tree_structure(kjd$gametree$parent, kjd$gametree$children, kjd$gametree$leaf),
  times = 5
)

microbenchmark(
  gogamer:::children_to_parentC(kjd$gametree$children),
  gogamer:::children_to_parentR(kjd$gametree$children),
  times = 5
)

microbenchmark(
  gogamer:::parent_to_childrenC(kjd$gametree$parent),
  gogamer:::parent_to_childrenR(kjd$gametree$parent),
  times = 5
)

