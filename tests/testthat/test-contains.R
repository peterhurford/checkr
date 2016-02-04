context("contains")

test_that("simple matchers", {
  expect_true(list(1, 2, 3) %contains_only% numeric)
  expect_true(list("a", "b", "c") %contains_only% character)
  expect_false(list("1", "2", "3") %contains% numeric)
  expect_false(list(1, 2, 3) %contains% character)
})

test_that("it is FALSE for empty lists", {
  expect_false(list("", "", "") %contains_only% character)
})

test_that("complex class matching", {
  obj <- structure("obj", class = "obj_class")
  expect_true(obj %is% obj_class)
  expect_true(list(obj, obj) %contains_only% obj_class)
})

describe("custom matchers", {
  test_that("simple_string", {
    expect_true(list("pizza", "a") %contains_only% simple_string)
    expect_false(list(list("a", "b")) %contains_only% simple_string)
  })
  test_that("double", {
    expect_true(list(1.0, 1) %contains_only% double)
  })
  test_that("vector", {
    expect_true(list(c(1, 2, 3), c("a", "b", "c")) %contains_only% vector)
  })
})

test_that("contains vs. contains_only", {
  expect_true(list(1, 2, "a") %contains% numeric) 
  expect_false(list(1, 2, "a") %contains_only% numeric) 
})
