library(testthat)
library(gogamer)

context("Tag parser")

test_that("Extract game properties", {
  sgf <- "(;GM[1]PW[Iyama]PB[Yamashita]RO[Final]EV[Kisei])"
  expect_equal(
    get_props(sgf, c("RO", "PW", "PB")),
    list(RO = "Final", PW = "Iyama", PB = "Yamashita")
  )
  expect_equal(
    get_props(sgf, c("RO", "PW", "ZZ")),
    list(RO = "Final", PW = "Iyama", ZZ = NA_character_)
  )
})


test_that("Extract game plays", {
  sgf <- "(;GM[1];B[pd];W[cq];B[dp];W[dq];B[fp])"
  expect_equal(
    get_moves(sgf),
    data.frame(color  = c(1, 2, 1, 2, 1),
               x      = c(16, 3, 4, 4, 6),
               y      = c(4, 17, 16, 17, 16),
               ismove = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  )
})

