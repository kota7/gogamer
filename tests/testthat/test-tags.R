library(testthat)
library(sgf)

context("Finding tags")

test_that("Extract properties unique to a game", {
  sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
  expect_equal(
    find_tags(sgf, c("RO", "PW", "PB")),
    c(RO = "Final", PW = "Iyama", PB = "Yamashita"))
  expect_equal(
    find_tags(sgf, c("RO", "PW", "ZZ")),
    c(RO = "Final", PW = "Iyama", ZZ = NA))
})

