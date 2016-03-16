context("present")

test_that("missing works", {
  fn <- function(x, y) {
    if (missing(x)) { x <- 1 }
    x + y
  }
  expect_equal(4, fn(1, 3))
  expect_equal(4, fn(y = 3))
  expect_error(fn(x = 3))
})

test_that("present is the opposite of missing", {
  fn <- function(x, y) {
    if (!present(x)) { x <- 1 }
    x + y
  }
  expect_equal(4, fn(1, 3))
  expect_equal(4, fn(y = 3))
  expect_error(fn(x = 3))
})

test_that("present can be used in the positive case", {
  fn <- function(x) {
    if (present(x)) { x <- 1 }
    if (missing(x)) { x <- 3 }
    x
  }
  expect_equal(1, fn(3))
  expect_equal(3, fn())
})

test_that("present can be used in a validation", {
  fn <- ensure(pre = present(x) && present(y),
    function(x = NULL, y = NULL) {
      if (missing(x)) { x <- 1 }
      x + y
    })
  expect_error(fn(y = 2), "Error on present(x) && present(y)", fixed = TRUE)
})
