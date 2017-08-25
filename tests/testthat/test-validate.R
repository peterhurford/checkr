context("validate")

test_that("simple errors", {
  expect_error(validate(1 %is% string), "1 %is% string")
  expect_error(validate("a" %is% double), "\"a\" %is% double")
  expect_error(validate(iris %is_not% dataframe), "iris %is_not% dataframe")
})

test_that("functions", {
  expect_true(validate(function(x) x %is% "function"))
  expect_true(validate(c %is% "function"))
  expect_true(validate(checkr:::validate %is% "function"))
  expect_true(validate(testthat::test_that %is% "function"))
})

test_that("simple errors with complex objects", {
  expect_error(validate( (a ~ b + c) %is_not% formula),
    "(a ~ b + c) %is_not% formula", fixed = TRUE)
  expect_error(validate(c("a", "b") %is% simple_string),
    "c(\"a\", \"b\") %is% simple_string", fixed = TRUE)
  expect_error(validate(list(1, 2, 3) %is_not% list),
    "list(1, 2, 3) %is_not% list", fixed = TRUE)
})

test_that("pre-conditions other than class matching", {
  expect_error(validate(1 + 1 == 3), "1 + 1 == 3", fixed = TRUE)
})

test_that("multiple pre-conditions", {
  expect_error(validate(1 %is% string, "a" %is% double),
    "1 %is% string, \"a\" %is% double")
  expect_error(validate(iris %is_not% dataframe, "a" %is_not% simple_string),
    "iris %is_not% dataframe, \"a\" %is_not% simple_string")
  expect_error(validate(iris %is_not% dataframe, NROW(iris) < 10),
    "iris %is_not% dataframe, NROW(iris) < 10", fixed = TRUE)
})

test_that("multiple matchers", {
  expect_error(validate(1 %is% c("string", "double")),
    "1 %is% c(\"string\", \"double\")", fixed = TRUE)
})

test_that("testing variables", {
  num <- 10
  str <- "pizza"
  expect_error(validate(num %is_not% numeric), "num %is_not% numeric")
  expect_error(validate(num %is_not% double), "num %is_not% double")
  expect_error(validate(str %is_not% character), "str %is_not% character")
  expect_error(validate(str %is_not% simple_string), "str %is_not% simple_string")
})
