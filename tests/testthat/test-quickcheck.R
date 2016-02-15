context("quickcheck")
library(validations)

describe("testing frame", {
  test_that("by default it has everything", {
    expect_true(length(list_classes(function_test_objects(identity)[[1]])) > 3)
  })
  test_that("it results in only integers if the function takes only integers", {
    add_one_int <- ensure(pre = x %is% integer, function(x) x + 1L)
    expect_true(function_test_objects(add_one_int)[[1]] %contains_only% integer)
  })
  test_that("it results in only doubles if the function takes only doubles", {
    add_one_double <- ensure(pre = x %is% double, function(x) x + 1.0)
    expect_true(function_test_objects(add_one_double)[[1]] %contains_only% double)
  })
  test_that("it results in both numerics if the function takes numerics", {
    add_one_number <- ensure(pre = x %is% numeric, function(x) x + 1)
    testing_frame <- function_test_objects(add_one_number)[[1]]
    expect_equal(2, length(list_classes(testing_frame)))
    expect_true(testing_frame %contains% integer)
    expect_true(testing_frame %contains% double)
  })
  test_that("it results in lists of all sorts but only lists if the function takes lists", {
    rev_list <- ensure(pre = x %is% list, rev)
    testing_frame <- function_test_objects(rev_list)[[1]]
    expect_true(testing_frame %contains_only% list)
  })
  test_that("it results in dataframes of all sorts but only dataframes", {
    rev_df <- ensure(pre = x %is% dataframe, rev)
    testing_frame <- function_test_objects(rev_df)[[1]]
    expect_true(testing_frame %contains_only% dataframe)
  })
  test_that("it results in both lists and vectors if the function takes that", {
    rev <- ensure(pre = x %is% list || x %is% vector, rev)
    testing_frame <- function_test_objects(rev)[[1]]
    expect_true(testing_frame %is% list)
    expect_true(testing_frame %contains% list)
    expect_true(testing_frame %contains% vector)
    expect_false(testing_frame %contains_only% list)
    expect_false(testing_frame %contains_only% vector)
  })
  test_that("it can further restrict based on things other than class", {
    add_positive <- ensure(pre = list(x %is% numeric, all(x > 0)), function(x) x + 1)
    testing_frame <- function_test_objects(add_positive)[[1]]
    expect_true(testing_frame %contains_only% numeric)
    expect_true(all(sapply(testing_frame, function(vec) all(na.omit(vec) > 0))))
  })
  test_that("it generates for two formals", {
    add_two <- ensure(pre = list(x %is% numeric, y %is% numeric), function(x, y) x + y)
    expect_equal(2, length(function_test_objects(add_two)))
  })
  test_that("it generates for three formals", {
    add_three <- ensure(pre = list(x %is% numeric, y %is% numeric, z %is% numeric),
      function(x, y, z) x + y + z)
    expect_equal(3, length(function_test_objects(add_three)))
  })
  test_that("it generates based on restrictions of each formal I", {
    add_three <- ensure(pre = list(x %is% numeric, y %is% numeric, z %is% numeric),
      function(x, y, z) x + y + z)
    testing_frame <- function_test_objects(add_three)
    lapply(testing_frame, function(frame) {
      expect_true(frame %contains% integer)
      expect_true(frame %contains% double)
      expect_true(frame %contains_only% numeric)
    })
  })
  test_that("it generates based on restrictions of each formal II", {
    random_string <- ensure(
      pre = list(length %is% numeric, length(length) == 1, length > 0,
        alphabet %is% list || alphabet %is% vector,
        alphabet %contains_only% simple_string),
      function(length, alphabet) {
        paste0(sample(alphabet, length, replace = TRUE), collapse = "")
      })
    testing_frame <- function_test_objects(random_string)
    expect_true(testing_frame$length %contains_only% numeric)
    expect_true(all(vapply(testing_frame$alphabet,
      function(alpha) alpha %contains_only% simple_string, logical(1))))
  })
  test_that("formals can be interdependent when necessary", {
    class_matcher <- ensure(pre = identical(class(x), class(y)), function(x, y) c(x, y))
    testing_frame <- function_test_objects(class_matcher)
    expect_equal(sapply(testing_frame$x, class), sapply(testing_frame$y, class))
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
  test_that("it errors if the testing frame is reduced to 0", {
    impossible_preconditions <- ensure(pre = list(x %is% character, x %isnot% character),
      identity)
    expect_error(quickcheck(impossible_preconditions), "impossible to satisfy")
  })
  test_that("reverse example", {
    quickcheck(ensure(pre = list(length(x) == 1, x %is% vector || x %is% list),
      post = identical(result, x), function(x) rev(x)))
    quickcheck(ensure(pre = list(x %is% vector || x %is% list),
      post = identical(result, x), function(x) rev(rev(x))))
  })
  test_that("random string example - failure", {
    random_string <- function(length, alphabet) {
      paste0(sample(alphabet, 10), collapse = "")
    }
    expect_error(quickcheck(ensure(
      pre = list(length %is% numeric, length(length) == 1, length > 0,
        alphabet %is% list || alphabet %is% vector,
          alphabet %contains_only% simple_string),
      post = list(nchar(result) == length, length(result) == 1,
        is.character(result), all(strsplit(result, "")[[1]] %in% alphabet)),
      random_string)), "Quickcheck for random_string failed on item #1")
  })
  test_that("random string example - success", {
    random_string <- function(length, alphabet) {
      paste0(sample(alphabet, length), collapse = "")
    }
    quickcheck(ensure(
      pre = list(length %is% numeric, length(length) == 1, length > 0,
        alphabet %is% list || alphabet %is% vector,
          alphabet %contains_only% simple_string),
      post = list(nchar(result) == length, length(result) == 1,
        is.character(result), all(strsplit(result, "")[[1]] %in% alphabet)),
      random_string))
  })
})
