library(testthat)
library(gogamer)
library(dplyr)

context("Read sample data")

test_that("mimiaka (installed data)", {
  x <- read_sgf(system.file("extdata/mimiaka.sgf", package = "gogamer"))
  expect_equal(x$mainbranchmoves, 325L)
  expect_equal(x$boardsize, 19L)

  y <- stateat(x, 127)
  expect_equal(y$b_captured, 5L)
  expect_equal(y$w_captured, 4L)
  expect_equal(y$lastmove, c(10L, 11L, 1L))

  y <- stateat(x, +Inf)
  expect_equal(y$movenumber, x$mainbranchmoves)

  expect_error(set_gamepath(x, 2))

  plotat(x, +Inf)
  kifuplot(x, 1, 99)
})



test_that("saikoyo (installed data)", {
  x <- read_sgf(system.file("extdata/saikoyo.sgf", package = "gogamer"))
  expect_equal(x$mainbranchmoves, 224L)
  expect_equal(x$boardsize, 19L)

  y <- stateat(x, 116)
  expect_equal(y$b_captured, 3L)
  expect_equal(y$w_captured, 0L)
  expect_equal(y$lastmove, c(8L, 9L, 2L))

  y <- stateat(x, +Inf)
  expect_equal(y$movenumber, x$mainbranchmoves)

  expect_error(set_gamepath(x, 2))

  plotat(x, +Inf)
  kifuplot(x, 1, 99)
})



test_that("branch", {
  x <- read_sgf(system.file("testdata/joseki.sgf", package = "gogamer"))
  expect_equal(length(x$gametree$leaf), 5L)

  y <- stateat(x, 1)
  expect_equal(y$comment, "Takamoku")
  y <- stateat(x, +Inf)
  expect_equal(y$comment, "Peaceful variation")

  y <- set_gamepath(x, 5) %>% stateat(+Inf)
  expect_equal(y$b_captured, 1L)

  expect_error(set_gamepath(x, 6))
})




test_that("points", {
  x <- read_sgf(system.file("testdata/multiend.sgf", package = "gogamer"))
  expect_equal(length(x$gametree$leaf), 3L)

  xlead <- numeric(3L)
  for (i in 1:3)
  {
    y <- set_gamepath(x, i) %>% stateat(+Inf)
    xlead[i] <- (sum(y$points$color == 1L) + y$w_captured) -
      (sum(y$points$color == 2L) + y$b_captured + x$komi)
  }
  expect_equal(xlead, c(3.5, 6.5, -0.5))

  # note. In all variation there is no dead stone on the board
  # so the xlead matches to the game outcome
})



test_that("kgs", {
  x <- read_sgf(system.file("testdata/kgs1.sgf", package = "gogamer"))
  expect_equal(x$blackrank, "1k")
  expect_equal(x$whiterank, "1d")
  expect_equal(nrow(x$comment), 3L)
  expect_equal(nrow(x$point), 0L)  # finishes with resign

  x <- read_sgf(system.file("testdata/kgs2.sgf", package = "gogamer"))
  y <- stateat(x, Inf)
  expect_equal(nrow(y$points), 113L)
  plot(y, markpoints = TRUE)
})


test_that("big file", {
  x <- read_sgf(system.file("testdata/move1e5.sgf.gz", package = "gogamer"))
  expect_equal(x$mainbranchmoves, 125899L)
  y <- stateat(x, Inf)
  expect_equal(y$b_captured, 62934L)
  expect_equal(y$w_captured, 62936L)


})

