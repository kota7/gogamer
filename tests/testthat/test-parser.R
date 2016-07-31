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


test_that("Extract game plays with setups", {
  sgf <- "(;GM[1];AB[pd][dp][pp];W[dc];B[cf];W[cd];B[dj])"
  expect_equal(
    get_moves(sgf),
    data.frame(color  = c(1, 1, 1, 2, 1, 2, 1),
               x      = c(16, 4, 16, 4, 3, 3, 4),
               y      = c(4, 16, 16, 3, 6, 4, 10),
               ismove = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  )

  sgf <- "(;GM[1];AB[bb:cd]AW[pq:sr])"
  expect_equal(
    get_moves(sgf) %>% dplyr::arrange_("color", "x", "y"),
    # sorting so that the same outcome is obtained
    data.frame(color  = c(rep(1, 6), rep(2, 8)),
               x      = c(2, 2, 2, 3, 3, 3, 16, 16, 17, 17, 18, 18, 19, 19),
               y      = c(2, 3, 4, 2, 3, 4, 17, 18, 17, 18, 17, 18, 17, 18),
               ismove = FALSE)
  )
})
