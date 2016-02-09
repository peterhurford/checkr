context("ensure")

#' Generate a random string.
#'
#' @param length numeric. The length of the random string to generate.
#' @param alphabet character. A list of characters to draw from to create the string.
random_string <- ensure(
  pre = list(length %is% numeric,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    length > 0),
  post = list(result %is% simple_string, nchar(result) == length),
  function(length, alphabet) {
    paste0(sample(alphabet, 10, replace = TRUE), collapse = "")
  })

test_that("the result is a validated function", {
  expect_true(random_string %is% "function")
  expect_true(random_string %is% validated_function)
})
test_that("validation preserves original classes", {
  add <- function(x, y) x + y
  class(add) <- "adding_function"
  expect_true(add %is% adding_function)
  add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric, add)
  expect_true(add %is% validated_function)
  expect_true(add %is% adding_function)
})
test_that("length is checked for numeric", {
  expect_error(random_string("pizza", LETTERS), "Error on length %is% numeric")
})
test_that("alphabet is checked for list or vector", {
  expect_error(random_string(10, "pizza"), 
    "Error on alphabet %is% list || alphabet %is% vector")
})
test_that("alphabet is checked that it only contains characters", {
  expect_error(random_string(10, list(1, 2, 3)),
    "Error on alphabet %contains_only% simple_string")
})
test_that("can have multiple errors", {
  expect_error(random_string(-10, list(1, 2, 3)),
    "Error on alphabet %contains_only% simple_string, length > 0")
})
test_that("function works with both pre- and post- checks", {
  expect_is(random_string(10, LETTERS), "character")
})

random_string <- ensure(
  pre = list(length %is% numeric,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    length > 0),
  # Use bogus post-conditions
  post = list(result %is% numeric, nchar(result) == length),
  function(length, alphabet) {
    paste0(sample(alphabet, 10, replace = TRUE), collapse = "")
  })
test_that("result is checked for numeric", {
  expect_error(random_string(10, LETTERS), "Error on result %is% numeric")
})

random_string <- ensure(
  pre = list(length %is% numeric,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    length > 0),
  # Use bogus post-conditions
  post = list(result %is% simple_string, nchar(result) < length),
  function(length, alphabet) {
    paste0(sample(alphabet, 10, replace = TRUE), collapse = "")
  })
test_that("result is checked for length", {
  expect_error(random_string(10, LETTERS), "Error on nchar(result) < length", fixed = TRUE)
})


test_that("it can have preconditions without postconditions", {
  add <- ensure(pre = list(x %is% numeric, y %is% numeric), function(x, y) x + y)
  expect_equal(add(1, 2), 3)
  expect_error(add("a", 2), "x %is% numeric")
  expect_error(add("a", "b"), "x %is% numeric, y %is% numeric")
})

test_that("it can have postconditions without preconditions", {
  add <- ensure(post = list(result %is% numeric), function(x, y) x + y)
  expect_equal(add(1, 2), 3)
  add <- ensure(post = list(result %is% character), function(x, y) x + y)
  expect_error(add(1, 2), "result %is% character")
})

test_that("a single postcondition does not have to be a list", {
  add <- ensure(post = result %is% numeric, function(x, y) x + y)
  expect_equal(add(1, 2), 3)
  add <- ensure(post = result %is% character, function(x, y) x + y)
  expect_error(add(1, 2), "result %is% character")
})

test_that("preconditions fetches the preconditions", {
  add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
    function(x, y) x + y)
  expect_identical(preconditions(add), substitute(list(x %is% numeric, y %is% numeric)))
})

test_that("postconditions fetches the preconditions", {
  add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
    function(x, y) x + y)
  expect_identical(postconditions(add), substitute(result %is% numeric))
})
