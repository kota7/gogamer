library(gogamer)
library(testthat)

context("is functions")

test_that("is functions", {
  x <- mimiaka
  y <- stateat(mimiaka, 10)
  z <- plotat(mimiaka, 10)
  w <- kifu(mimiaka)
  v <- kifuplot(mimiaka)

  expect_true(is.gogame(x))
  expect_true(is.gostate(y))
  expect_true(is.ggoban(z))
  expect_true(is.gokifu(w))
  expect_true(is.ggkifu(v))

  expect_false(is.gogame(y))
  expect_false(is.gostate(z))
  expect_false(is.ggoban(w))
  expect_false(is.gokifu(v))
  expect_false(is.ggkifu(x))

  expect_false(is.gogame(z))
  expect_false(is.gostate(w))
  expect_false(is.ggoban(v))
  expect_false(is.gokifu(x))
  expect_false(is.ggkifu(y))

  expect_false(is.gogame(w))
  expect_false(is.gostate(v))
  expect_false(is.ggoban(x))
  expect_false(is.gokifu(y))
  expect_false(is.ggkifu(z))

  expect_false(is.gogame(v))
  expect_false(is.gostate(x))
  expect_false(is.ggoban(y))
  expect_false(is.gokifu(z))
  expect_false(is.ggkifu(w))

  # kifuplot with no note inherits ggoban
  expect_true(is.ggoban(kifuplot(mimiaka, 1, 5)))
})
