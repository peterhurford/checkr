context("ensure")

#' Generate a random string.
#'
#' @param length numeric. The length of the random string to generate.
#' @param alphabet character. A list of characters to draw from to create the string.
random_string <- ensure(
  pre = list(length %is% numeric, length(length) == 1, length > 0,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    all(sapply(alphabet, nchar) == 1)),
  post = list(result %is% simple_string, nchar(result) == length),
  function(length, alphabet) {
    paste0(sample(alphabet, length, replace = TRUE), collapse = "")
  })

describe("classes", {
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
})

describe("precondition validations", {
  test_that("length is checked for numeric", {
    expect_error(random_string("pizza", LETTERS), "Error on length %is% numeric")
  })
  test_that("alphabet is checked for existence", {
    expect_error(random_string(10), "Error on missing arguments: alphabet")
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
      "Error on length > 0, alphabet %contains_only% simple_string")
  })
  test_that("default args are also checked", {
    add_default <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
      function(x, y = "a") x + y)
    expect_equal(add_default(4, 5), 9)
    expect_error(add_default(4), "Error on y %is% numeric")
  })
})

describe("postconditions", {
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
})

describe("one without the other", {
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
})

describe("fetchers", {
  add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
    function(x, y) x + y)
  test_that("preconditions fetches the preconditions", {
    expect_identical(preconditions(add), substitute(list(x %is% numeric, y %is% numeric)))
  })
  test_that("postconditions fetches the preconditions", {
    expect_equal(postconditions(add), substitute(result %is% numeric))
  })
  test_that("postconditions fetches the preconditions", {
    expect_equal(get_prevalidated_fn(add), function(x, y) x + y)
  })
})

describe("passing conditions", {
  test_that("random_string works with both pre- and post- checks", {
    rand_string <- random_string(10, LETTERS)
    expect_is(rand_string, "character")
    expect_true(nchar(rand_string) == 10)
  })
  test_that("random_string works with both pre- and post- checks and explicit formals", {
    rand_string <- random_string(length = 10, alphabet = LETTERS)
    expect_is(rand_string, "character")
    expect_true(nchar(rand_string) == 10)
  })
  test_that("add works with both pre- and post- checks", {
    add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
      function(x, y) x + y)
    expect_equal(add(4, 5), 9)
    expect_equal(add(4L, 5L), 9L)
  })
  test_that("add works with both pre- and post- checks and explicit formals", {
    add <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
      function(x, y) x + y)
    expect_equal(add(x = 4, y = 5), 9)
    expect_equal(add(x = 4L, y = 5L), 9L)
  })
  test_that("function works with a default argument", {
    add_default <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
      function(x, y = 1) x + y)
    expect_equal(add_default(4, 5), 9)
    expect_equal(add_default(4), 5)
  })
  test_that("function works with a default argument and explicit formals", {
    add_default <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric,
      function(x, y = 1) x + y)
    expect_equal(add_default(x = 4, y = 5), 9)
    expect_equal(add_default(x = 4), 5)
  })
  test_that("can't validate twice", {
    expect_error(ensure(pre = x %is% numeric, random_string), "already been validated")
  })
})

describe("present", {
  test_that("present can be used in a validation", {
    fn <- ensure(pre = present(x) && present(y),
      function(x = NULL, y = NULL) {
        if (missing(x)) { x <- 1 }
        if (missing(y)) { y <- 1 }
        x + y
      })
    expect_error(fn(y = 2), "Error on present(x) && present(y)", fixed = TRUE)
  })
  test_that("present can be used in a validation II", {
    fn <- ensure(pre = list(present(x), present(y)),
      function(x = NULL, y = NULL) {
        if (missing(x)) { x <- 1 }
        if (missing(y)) { y <- 1 }
        x + y
      })
    expect_error(fn(x = 2), "Error on present(y)", fixed = TRUE)
    expect_error(fn(y = 2), "Error on present(x)", fixed = TRUE)
  })
  test_that("present can be used in a validation III", {
    fn <- ensure(pre = list(present(x), present(y)),
      function(x = NULL, y = NULL) {
        if (missing(x)) { x <- 1 }
        if (missing(y)) { y <- 1 }
        x + y
      })
    expect_error(fn(x = 2), "Error on present(y)", fixed = TRUE)
    expect_error(fn(y = 2), "Error on present(x)", fixed = TRUE)
  })
  test_that("present can be used in a validation IV", {
    fn <- ensure(pre = present(x),
      function(x = NULL, y = NULL) {
        if (missing(x)) { x <- 1 }
        if (missing(y)) { y <- 1 }
        x + y
      })
    expect_equal(3, fn(x = 2))
    expect_error(fn(y = 2), "Error on present(x)", fixed = TRUE)
  })
})

describe("missing arguments", {
  fn <- ensure(pre = list(
    a %is% "NULL" || a %is% list,
    b %is% "NULL" || b %is% list),
     function(a = NULL, b = NULL, c = NULL) { c(a, b, c) })
  test_that("the function works", {
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), c = list(3)))
  })
  test_that("c can be missing", {
    expect_equal(list(1, 2), fn(a = list(1), b = list(2)))
  })
  test_that("b cannot be missing", {
    expect_error(fn(a = list(1), c = list(2)), "Error on missing arguments: b")
  })
  test_that("a cannot be missing", {
    expect_error(fn(b = list(1), c = list(2)), "Error on missing arguments: a")
  })
})
