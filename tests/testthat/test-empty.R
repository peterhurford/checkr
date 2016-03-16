context("is.empty")

test_that("it is TRUE for NULL, NA, and empty string", {
  expect_true(is.empty(NULL))
  expect_true(is.empty(NA))
  expect_true(is.empty(""))
})

test_that("it is TRUE for length-0 objects", {
  expect_true(is.empty(list()))
  expect_true(is.empty(c()))
  expect_true(is.empty(data.frame()))
  expect_true(is.empty(logical(0)))
})

test_that("it is TRUE for lists and vectors that combine the above", {
  expect_true(is.empty(list(NA, NA)))
  expect_true(is.empty(list(NA, NULL, NA)))
  expect_true(is.empty(list(list())))
  expect_true(is.empty(list(a = list())))
  expect_true(is.empty(list(a = list(NA, NULL), b = list(NA))))
})

test_that("it is FALSE for everything else", {
  expect_false(is.empty("a"))
  expect_false(is.empty(1))
  expect_false(is.empty(iris))
  expect_false(is.empty(list(NA, NULL, 1)))
  expect_false(is.empty(list(a = list(NA, NA), b = list(1, 2))))
})
