context("validate")

test_that("simple errors", {
  expect_error(validate(1 %is% string), "1 %is% string")
  expect_error(validate("a" %is% double), "\"a\" %is% double")
  expect_error(validate(iris %isnot% dataframe), "iris %isnot% dataframe")
})

test_that("simple errors with complex objects", {
  expect_error(validate((a ~ b + c) %isnot% formula),
    "(a ~ b + c) %isnot% formula", fixed = TRUE)
  expect_error(validate(c("a", "b") %is% simple_string),
    "c(\"a\", \"b\") %is% simple_string", fixed = TRUE)
  expect_error(validate(list(1, 2, 3) %isnot% list),
    "list(1, 2, 3) %isnot% list", fixed = TRUE)
})

test_that("conditions other than class matching", {
  expect_error(validate(1 + 1 == 3), "1 + 1 == 3", fixed = TRUE)
})

test_that("multiple conditions", {
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

describe("it works in a concrete example", {
  random_string <- function(length, alphabet) {
    validate(length %is% numeric,
      alphabet %is% list || alphabet %is% vector,
      alphabet %contains_only% character,
      length > 0)
    paste0(sample(alphabet, 10, replace = TRUE), collapse = "")
  }
  test_that("the sample function works", {
    expect_true(random_string(10, LETTERS) %is% "simple_string")
    expect_true(nchar(random_string(10, LETTERS)) == 10)
  })
  test_that("length is validated for class", {
    expect_error(random_string("pizza", LETTERS),
      "Failed conditions: length %is% numeric")
  })
  test_that("alphabet is validated for class", {
    expect_error(random_string(10, iris),
      "Failed conditions: alphabet %is% list")
  })
  test_that("alphabet is validated for membership", {
    expect_error(random_string(10, list(1, 2, 3)),
      "Failed conditions: alphabet %contains_only% character")
  })
  test_that("length is validated for length", {
    expect_error(random_string(-10, LETTERS),
      "Failed conditions: length > 0")
  })
})
