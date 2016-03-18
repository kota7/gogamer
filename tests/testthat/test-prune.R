library(testthat)
library(sgf)

context("Pruning branches")

test_that("No branch", {
  sgf <- "(;GM[1];B[pq];W[dd])"
  expect_equal(
    prune_sgf(sgf), ";GM[1];B[pq];W[dd]")
})


test_that("Signle layer", {
  sgf <- "(;GM[1];B[pq];W[dd](;B[cp];W[pg])(;B[cq];W[pp]))"
  expect_equal(
    prune_sgf(sgf), ";GM[1];B[pq];W[dd];B[cp];W[pg]")
})


test_that("Multi layer", {
  sgf <- "(;GM[1];B[pq];W[dd](;B[cp];W[pg](;B[cq])(;B[cc];W[pp])))"
  expect_equal(
    prune_sgf(sgf), ";GM[1];B[pq];W[dd];B[cp];W[pg];B[cq]")
})


test_that("Disturbing characters", {
  sgf <- "(;GM[1];B[pq]C[Lee Sedol [9d\\] (Korea) ];W[dd](;B[cp];W[pg](;B[cq])(;B[cc];W[pp])))"
  expect_equal(
    prune_sgf(sgf),
    ";GM[1];B[pq]C[Lee Sedol [9d\\] (Korea) ];W[dd];B[cp];W[pg];B[cq]")
})
