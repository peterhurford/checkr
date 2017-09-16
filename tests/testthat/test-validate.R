context("validate")

test_that("simple errors", {
  expect_error(validate(1 %is% string), "1 %is% string")
  expect_error(validate("a" %is% double), "\"a\" %is% double")
  expect_error(validate(iris %isnot% dataframe), "iris %isnot% dataframe")
})

test_that("functions", {
  expect_true(validate(function(x) x %is% "function"))
  expect_true(validate(c %is% "function"))
  expect_true(validate(checkr:::validate %is% "function"))
  expect_true(validate(testthat::test_that %is% "function"))
})

test_that("simple errors with complex objects", {
  expect_error(validate((a ~ b + c) %isnot% formula),
    "(a ~ b + c) %isnot% formula", fixed = TRUE)
  expect_error(validate(c("a", "b") %is% simple_string),
    "c(\"a\", \"b\") %is% simple_string", fixed = TRUE)
  expect_error(validate(list(1, 2, 3) %isnot% list),
    "list(1, 2, 3) %isnot% list", fixed = TRUE)
})

test_that("pre-conditions other than class matching", {
  expect_error(validate(1 + 1 == 3), "1 + 1 == 3", fixed = TRUE)
})

test_that("multiple pre-conditions", {
  expect_error(validate(1 %is% string, "a" %is% double),
    "1 %is% string, \"a\" %is% double")
  expect_error(validate(iris %isnot% dataframe, "a" %isnot% simple_string),
    "iris %isnot% dataframe, \"a\" %isnot% simple_string")
  expect_error(validate(iris %isnot% dataframe, NROW(iris) < 10),
    "iris %isnot% dataframe, NROW(iris) < 10", fixed = TRUE)
})

test_that("multiple matchers", {
  expect_error(validate(1 %is% c("string", "double")),
    "1 %is% c(\"string\", \"double\")", fixed = TRUE)
})

test_that("testing variables", {
  num <- 10
  str <- "pizza"
  expect_error(validate(num %isnot% numeric), "num %isnot% numeric")
  expect_error(validate(num %isnot% double), "num %isnot% double")
  expect_error(validate(str %isnot% character), "str %isnot% character")
  expect_error(validate(str %isnot% simple_string), "str %isnot% simple_string")
})
