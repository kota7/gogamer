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


test_that("Extract game plays", {
  sgf <- "(;GM[1];B[pd];W[cq];B[dp];W[dq];B[fp])"
  expect_equal(
    gogamer:::get_moves(sgf),
    data.frame(color  = c(1L, 2L, 1L, 2L, 1L),
               x      = c(16L, 3L, 4L, 4L, 6L),
               y      = c(4L, 17L, 16L, 17L, 16L),
               ismove = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  )
})


test_that("Extract game plays with setups", {
  sgf <- "(;GM[1];AB[pd][dp][pp];W[dc];B[cf];W[cd];B[dj])"
  expect_equal(
    gogamer:::get_moves(sgf),
    data.frame(color  = c(1L, 1L, 1L, 2L, 1L, 2L, 1L),
               x      = c(16L, 4L, 16L, 4L, 3L, 3L, 4L),
               y      = c(4L, 16L, 16L, 3L, 6L, 4L, 10L),
               ismove = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  )

  sgf <- "(;GM[1];AB[bb:cd]AW[pq:sr])"
  expect_equal(
    gogamer:::get_moves(sgf) %>% dplyr::arrange_("color", "x", "y"),
    # sorting so that the same outcome is obtained
    data.frame(color  = c(rep(1L, 6L), rep(2L, 8L)),
               x      = c(2L, 2L, 2L, 3L, 3L, 3L, 16L,
                          16L, 17L, 17L, 18L, 18L, 19L, 19L),
               y      = c(2L, 3L, 4L, 2L, 3L, 4L, 17L, 18L,
                          17L, 18L, 17L, 18L, 17L, 18L),
               ismove = FALSE)
  )
})
