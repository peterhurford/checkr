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
  expect_equal(1, fn(x = 5))
  expect_equal(1, fn(x = NULL))
  expect_equal(3, fn())
})

test_that("present only tests variables in the scope and doesn't get confused", {
  fn <- function(mean) {  # Could get confused for the funtion `mean`.
    if (present(mean)) { mean <- 1 }
    if (missing(mean)) { mean <- 3 }
    mean
  }
  expect_equal(1, fn(mean = 5))
  expect_equal(1, fn(mean = NULL))
  expect_equal(3, fn())
})

test_that("present only tests variables in the scope and doesn't get confused II", {
  a <- 1
  fn <- function(b) {
    expect_false(present(a))
    expect_true(present(b))
  }
  fn(1)
})
