library(testthat)
library(gogamer)

context("Pruning branches")

test_that("No branch", {
  sgf <- "(;GM[1];B[pq];W[dd])"

  expect_equal(
    prune_sgf(sgf, keep_first = TRUE), ";GM[1];B[pq];W[dd]")
  expect_equal(
    prune_sgf(sgf, keep_first = FALSE), ";GM[1];B[pq];W[dd]")
})


test_that("Signle layer", {
  sgf <- "(;GM[1];B[pq];W[dd](;B[cp];W[pg])(;B[cq];W[pp]))"

  expect_equal(
    prune_sgf(sgf, keep_first = TRUE), ";GM[1];B[pq];W[dd];B[cp];W[pg]")
  expect_equal(
    prune_sgf(sgf, keep_first = FALSE), ";GM[1];B[pq];W[dd];B[cq];W[pp]")
})


test_that("Multi layer", {
  sgf <- "(;GM[1];B[pq];W[dd](;B[cp];W[pg](;B[cq])(;B[cc];W[pp])))"

  expect_equal(
    prune_sgf(sgf, keep_first = TRUE),
    ";GM[1];B[pq];W[dd];B[cp];W[pg];B[cq]")
  expect_equal(
    prune_sgf(sgf, keep_first = FALSE),
    ";GM[1];B[pq];W[dd];B[cp];W[pg];B[cc];W[pp]")
})


test_that("Disturbing characters", {
  sgf <- "(;GM[1];B[pq]C[Lee Sedol [9d\\] (Korea) ];W[dd](;B[cp];W[pg]C[Iyama Yuta [9d\\] (Japan)](;B[cq])(;B[cc];W[pp]))(;B[cc];W[pp]C[Yuta Iyama [9d\\] (Japan)];B[ce]))"

  expect_equal(
    prune_sgf(sgf, keep_first = TRUE),
    ";GM[1];B[pq]C[Lee Sedol [9d\\] (Korea) ];W[dd];B[cp];W[pg]C[Iyama Yuta [9d\\] (Japan)];B[cq]")
  expect_equal(
    prune_sgf(sgf, keep_first = FALSE),
    ";GM[1];B[pq]C[Lee Sedol [9d\\] (Korea) ];W[dd];B[cc];W[pp]C[Yuta Iyama [9d\\] (Japan)];B[ce]")
})



