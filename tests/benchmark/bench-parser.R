library(rbenchmark)
library(sgf)

sgf <- readLines("tests/sample/base.sgf") %>% paste0(collapse = "\n")

tags <- c("PW", "PB", "RE", "DT", "SZ")
benchmark(
  get_props(sgf, tags), get_props(rep(sgf, 10), tags),
  get_props(rep(sgf, 1e2), tags), get_props(rep(sgf, 1e3), tags),
  get_props(rep(sgf, 1e4), tags),
  replications = 1)

benchmark(
  get_moves(sgf), get_moves(rep(sgf, 10)),
  get_moves(rep(sgf, 1e2)), get_moves(rep(sgf, 1e3)),
  get_moves(rep(sgf, 1e4)),
  replications = 1)
