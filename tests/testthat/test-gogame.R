library(testthat)
library(gogamer)

context("Tag parser")

test_that("gogame constructor", {

  # handicap property does not match the number of setup moves
  properties <- list(whitename = "", whiterank = "",
                     blackname = "", blackrank = "",
                     boardsize = 19, komi = 0, handicap = 3,
                     date = NA_character_, event = "",
                     result = "", rule = NA_character_)
  moves <- data.frame(color = c(1L, 1L, 2L),
                      x = c(16L, 4L, 10L), y = c(4L, 16L, 10L),
                      ismove = c(FALSE, FALSE, TRUE))
  expect_warning(gogame(properties, moves))
})
