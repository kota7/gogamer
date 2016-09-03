library(testthat)
library(gogamer)

context("Tag parser")

test_that("Extract game properties", {
  sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
  expect_equal(
    gogamer:::get_props(sgf, c("RO", "PW", "PB")),
    list(RO = "Final", PW = "Iyama", PB = "Yamashita")
  )
  expect_equal(
    gogamer:::get_props(sgf, c("RO", "PW", "ZZ")),
    list(RO = "Final", PW = "Iyama", ZZ = NA_character_)
  )
})

