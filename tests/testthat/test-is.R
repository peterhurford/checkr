context("is")

test_that("simple matchers", {
  expect_true(1 %is% numeric)
  expect_true(1L %is% integer)
  expect_true("a" %is% character)
  expect_false("a" %is% numeric)
  expect_false(1 %is% character)
})

test_that("more complex classes", {
  expect_true(iris %is% data.frame)
  expect_true(list(1, 2, 3) %is% list)
  expect_true( (a ~ b + c) %is% formula)
})

test_that("complex class matching", {
  obj <- structure("obj", class = "obj_class")
  expect_true(obj %is% obj_class)
})

test_that("no overlap in classes", {
  expect_true(is.list(iris))
  expect_false(iris %is% list)  # because it is a dataframe
})

test_that("custom class names", {
  expect_true("a" %is% string)
  expect_true(iris %is% dataframe)
  expect_true(NULL %is% NULL)
})

describe("custom matchers", {
  test_that("simple_string", {
    expect_true("pizza" %is% simple_string)
  })
  test_that("double", {
    expect_true(1.0 %is% double)
    expect_true(1 %is% double)
    expect_false(1L %is% double)
  })
  test_that("vector", {
    expect_true(c(1, 2, 3) %is% vector)
    expect_true(c("a", "b", "c") %is% vector)
    expect_true(c(a = "a", b = "b", c = "c") %is% vector)
    expect_false(list(1, 2, 3) %is% vector)
    expect_false(iris %is% vector)
  })
  test_that("empty", {
    expect_true("" %is% empty)
    expect_true(NA %is% empty)
    expect_true(NULL %is% empty)
    expect_false(1 %is% empty)
    expect_false("a" %is% empty)
    expect_false(iris %is% empty)
  })
  test_that("NA", {
    expect_true(NA %is% NA)
    expect_true(NA_character_ %is% NA)
    expect_true(NA_integer_ %is% NA)
    expect_false("" %is% NA)
    expect_false(NULL %is% NA)
  })
})

test_that("multiple matchers", {
  expect_false("a" %is% c("character", "numeric"))
  expect_true(structure("a", class = c("character", "numeric")) %is% c("character", "numeric"))
})

test_that("isnot negates is", {
  expect_true("a" %isnot% numeric)
  expect_true(1 %isnot% string)
  expect_false(1 %isnot% numeric)
  expect_false("a" %isnot% string)
})
