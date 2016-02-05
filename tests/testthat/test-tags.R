library(testthat)
library(sgf)

context("Tag parser")

test_that("Extract game properties", {
  sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
  expect_equal(
    get_props(sgf, c("RO", "PW", "PB"))[[1]],
    c(RO = "Final", PW = "Iyama", PB = "Yamashita"))
  expect_equal(
    get_props(sgf, c("RO", "PW", "ZZ"))[[1]],
    c(RO = "Final", PW = "Iyama", ZZ = NA))
})

