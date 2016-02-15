context("quickcheck")
library(validations)

describe("testing frame", {
  test_that("by default it has everything", {
    expect_true(length(list_classes(function_test_objects(identity))) > 3)
  })
  test_that("it results in only integers if the function takes only integers", {
    add_one_int <- ensure(pre = x %is% integer, function(x) x + 1L)
    expect_true(function_test_objects(add_one_int) %contains_only% integer)
  })
  test_that("it results in only doubles if the function takes only doubles", {
    add_one_double <- ensure(pre = x %is% double, function(x) x + 1.0)
    expect_true(function_test_objects(add_one_double) %contains_only% double)
  })
  test_that("it results in both numerics if the function takes numerics", {
    add_one_number <- ensure(pre = x %is% numeric, function(x) x + 1)
    testing_frame <- function_test_objects(add_one_number)
    expect_equal(2, length(list_classes(testing_frame)))
    expect_true(testing_frame %contains% integer)
    expect_true(testing_frame %contains% double)
  })
  test_that("it results in lists of all sorts but only lists if the function takes lists", {
    expect_true(FALSE)
  })
  test_that("it results in dataframes of all sorts but only dataframes", {
    expect_true(FALSE)
  })
  test_that("it results in lists or vectors if the function takes that", {
    expect_true(FALSE)
  })
  test_that("it can further restrict based on things other than class", {
    expect_true(FALSE)
  })
})

describe("quickcheck", {
  test_that("simple success example I", {
    quickcheck(identity)
  })
  test_that("simple success example II", {
    add_one <- ensure(
      pre = x %is% numeric,
      post = result %is% numeric,
      function(x) x + 1) 
    quickcheck(add_one)
  })
  test_that("simple failure example I", {
    add_one <- ensure(
      pre = x %is% numeric,
      post = result %is% character, # this will fail because the result will actually be numeric
      function(x) x + 1) 
    expect_error(quickcheck(add_one), "Quickcheck for add_one failed on item #1")
  })
  test_that("simple failure example II", {
    add_one <- ensure(
      pre = x %is% character, # quickcheck will only generate characters which will fail
      post = result %is% numeric,
      function(x) x + 1) 
    expect_error(quickcheck(add_one), "Quickcheck for add_one failed on item #1")
  })
  test_that("succeeding based on a specified postcondition", {
    expect_true(FALSE)
  })
  test_that("failing based on a specified postcondition", {
    expect_true(FALSE)
  })
  test_that("it errors if the testing frame is reduced to 0", {
    expect_true(FALSE)
  })
  test_that("reverse example", {
    expect_true(FALSE)
  })
})
