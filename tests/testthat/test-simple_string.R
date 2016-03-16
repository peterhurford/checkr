context("is.simple_string")

test_that("it is TRUE for length-1 non-empty strings", {
  expect_true(is.simple_string("pizza"))
  expect_true(is.simple_string("paul"))
  expect_true(is.simple_string("having spaces is okay too"))
  expect_true(is.simple_string("also /slashes and nonAlph@nUm3ric char$"))
})

test_that("it is FALSE for length >1 non-empty strings", {
  expect_false(is.simple_string(c("two", "strings")))
  expect_false(is.simple_string(c("a", "vector", "with", "five", "strings")))
})

test_that("it is FALSE for length-1 empty strings", {
  expect_false(is.simple_string(NA))
  expect_false(is.simple_string(NULL))
})

test_that("it is FALSE for length-1 non-empty non-strings", {
  expect_false(is.simple_string(FALSE))
  expect_false(is.simple_string(iris))
})
