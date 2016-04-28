context("ensure")

# Used for finding a formal test
CONSTANT_NUMBER <- 10

#' Generate a random string.
#'
#' @param length numeric. The length of the random string to generate.
#' @param alphabet character. A list of characters to draw from to create the string.
random_string <- ensure(
  pre = list(length %is% numeric, length(length) == 1, length > 0, length < 1e+7,
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
  test_that("validation preserves original formals", {
    add <- function(x, y) x + y
    eadd <- ensure(pre = list(x %is% numeric, y %is% numeric), post = result %is% numeric, add)
    expect_equal(formals(add), formals(eadd))
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

  test_that("it works for NULL", {
    fn <- ensure(post = result %is% NULL, function(x) NULL)
    expect_equal(NULL, fn(1))
    expect_equal(NULL, fn("a"))
    expect_equal(NULL, fn(NULL))
  })
  
  test_that("it works for an empty string", {
    fn <- ensure(post = identical(result, ""), function(x) "")
    expect_equal("", fn(1))
    expect_equal("", fn("a"))
    expect_equal("", fn(NULL))
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
  test_that("postconditions fetches the postconditions", {
    expect_equal(postconditions(add), substitute(result %is% numeric))
  })
  test_that("get_prevalidated_fn gets the pre-validated function", {
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
    fn <- ensure(pre = !(present(x) && present(y)),
      function(x, y) {
        if (missing(x)) { x <- 1 }
        if (missing(y)) { y <- 1 }
        x + y
      })
    expect_equal(3, fn(x = 2))
    expect_equal(3, fn(y = 2))
    expect_error(fn(x = 2, y = 2), "Error on !(present(x) && present(y))", fixed = TRUE)
  })
})

describe("missing arguments I", {
  fn <- ensure(pre = list(a %is% list, b %is% list),
    function(a, b, c = NULL) { c(a, b, c) })
  test_that("the function works I", {
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), c = list(3)))
  })
  test_that("the function works without names I", {
    expect_equal(list(1, 2, 3), fn(list(1), list(2), list(3)))
  })
  test_that("the function works with partial names I", {
    expect_equal(list(1, 2, 3), fn(a = list(1), list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), list(3)))
  })
  test_that("the function works in the opposite order I", {
    expect_equal(list(1, 2, 3), fn(a = list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(b = list(2), c = list(3), a = list(1)))
    expect_equal(list(1, 2, 3), fn(b = list(2), a = list(1), c = list(3)))
    expect_equal(list(1, 2, 3), fn(c = list(3), b = list(2), a = list(1)))
  })
  test_that("c can be missing I", {
    expect_equal(list(1, 2), fn(a = list(1), b = list(2)))
    expect_equal(list(1, 2), fn(list(1), b = list(2)))
    expect_equal(list(1, 2), fn(a = list(1), list(2)))
    expect_equal(list(1, 2), fn(list(1), list(2)))
  })
  test_that("silence I", {
    expect_silent(fn(list(1), b = list(2)))
  })
  test_that("c can be missing in the opposite order I", {
    expect_equal(list(1, 2), fn(b = list(2), a = list(1)))
  })
  test_that("b cannot be missing I", {
    expect_error(fn(a = list(1), c = list(2)), "Error on missing arguments: b")
    expect_error(fn(list(1), c = list(2)), "Error on missing arguments: b")
  })
  test_that("a cannot be missing I", {
    expect_error(fn(b = list(1), c = list(2)), "Error on missing arguments: a")
  })
})

describe("missing arguments II", {
  fn <- ensure(pre = list(a %is% list, b %is% list),
    function(a, b, c) {
      if (missing(c)) { c <- 1 }
      c(a, b, c)
    })
  test_that("the function works II", {
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), c = list(3)))
  })
  test_that("the function works without names II", {
    expect_equal(list(1, 2, 3), fn(list(1), list(2), list(3)))
  })
  test_that("the function works with partial names II", {
    expect_equal(list(1, 2, 3), fn(a = list(1), list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), list(3)))
  })
  test_that("the function works in the opposite order II", {
    expect_equal(list(1, 2, 3), fn(a = list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(b = list(2), c = list(3), a = list(1)))
    expect_equal(list(1, 2, 3), fn(b = list(2), a = list(1), c = list(3)))
    expect_equal(list(1, 2, 3), fn(c = list(3), b = list(2), a = list(1)))
  })
  test_that("c can be missing II", {
    expect_equal(list(1, 2, 1), fn(a = list(1), b = list(2)))
    expect_equal(list(1, 2, 1), fn(list(1), b = list(2)))
    expect_equal(list(1, 2, 1), fn(a = list(1), list(2)))
    expect_equal(list(1, 2, 1), fn(list(1), list(2)))
  })
  test_that("silence II", {
    expect_silent(fn(list(1), b = list(2)))
  })
  test_that("c can be missing in the opposite order II", {
    expect_equal(list(1, 2, 1), fn(b = list(2), a = list(1)))
  })
  test_that("b cannot be missing II", {
    expect_error(fn(a = list(1), c = list(2)), "Error on missing arguments: b")
    expect_error(fn(list(1), c = list(2)), "Error on missing arguments: b")
  })
  test_that("a cannot be missing II", {
    expect_error(fn(b = list(1), c = list(2)), "Error on missing arguments: a")
  })
})

describe("missing arguments III", {
  fn <- ensure(pre = list(
    if (present(a)) { a %is% list },
    if (present(b)) { b %is% list },
    if (present(c)) { c %is% list }),
    function(a, b, c) {
      if (missing(a)) { a <- NULL }
      if (missing(b)) { b <- NULL }
      if (missing(c)) { c <- NULL }
      c(a, b, c)
    })
  test_that("the function works III", {
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), c = list(3)))
  })
  test_that("the function works without names III", {
    expect_equal(list(1, 2, 3), fn(list(1), list(2), list(3)))
  })
  test_that("the function works with partial names III", {
    expect_equal(list(1, 2, 3), fn(a = list(1), list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), list(3)))
  })
  test_that("the function works in the opposite order III", {
    expect_equal(list(1, 2, 3), fn(a = list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(b = list(2), c = list(3), a = list(1)))
    expect_equal(list(1, 2, 3), fn(b = list(2), a = list(1), c = list(3)))
    expect_equal(list(1, 2, 3), fn(c = list(3), b = list(2), a = list(1)))
  })
  test_that("c can be missing III", {
    expect_equal(list(1, 2), fn(a = list(1), b = list(2)))
    expect_equal(list(1, 2), fn(list(1), b = list(2)))
    expect_equal(list(1, 2), fn(a = list(1), list(2)))
    expect_equal(list(1, 2), fn(list(1), list(2)))
  })
  test_that("silence III", {
    expect_silent(fn(list(1), b = list(2)))
  })
  test_that("c can be missing in the opposite order III", {
    expect_equal(list(1, 2), fn(b = list(2), a = list(1)))
  })
  test_that("b can be missing III", {
    expect_equal(list(1, 2), fn(a = list(1), c = list(2)))
    expect_equal(list(1, 2), fn(list(1), c = list(2)))
  })
  test_that("b can be missing in the opposite order III", {
    expect_equal(list(1, 2), fn(c = list(2), a = list(1)))
  })
  test_that("a can be missing III", {
    expect_equal(list(1, 2), fn(b = list(1), c = list(2)))
  })
  test_that("a can be missing in the opposite order III", {
    expect_equal(list(1, 2), fn(c = list(2), b = list(1)))
  })
})

describe("missing arguments IV", {
  fn <- ensure(pre = list(
    a %is% list || a %is% NULL,
    b %is% list || b %is% NULL,
    c %is% list || c %is% NULL),
    function(a = NULL, b = NULL, c = NULL) { c(a, b, c) })
  test_that("the function works IV", {
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), c = list(3)))
  })
  test_that("the function works without names IV", {
    expect_equal(list(1, 2, 3), fn(list(1), list(2), list(3)))
  })
  test_that("the function works with partial names IV", {
    expect_equal(list(1, 2, 3), fn(a = list(1), list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(list(1), b = list(2), c = list(3)))
    expect_equal(list(1, 2, 3), fn(a = list(1), b = list(2), list(3)))
  })
  test_that("the function works in the opposite order IV", {
    expect_equal(list(1, 2, 3), fn(a = list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(list(1), c = list(3), b = list(2)))
    expect_equal(list(1, 2, 3), fn(b = list(2), c = list(3), a = list(1)))
    expect_equal(list(1, 2, 3), fn(b = list(2), a = list(1), c = list(3)))
  })
  test_that("c can be missing IV", {
    expect_equal(list(1, 2), fn(a = list(1), b = list(2)))
    expect_equal(list(1, 2), fn(list(1), b = list(2)))
    expect_equal(list(1, 2), fn(a = list(1), list(2)))
    expect_equal(list(1, 2), fn(list(1), list(2)))
  })
  test_that("silence IV", {
    expect_silent(fn(list(1), b = list(2)))
  })
  test_that("c can be missing in the opposite order IV", {
    expect_equal(list(1, 2), fn(b = list(2), a = list(1)))
  })
  test_that("b can be missing IV", {
    expect_equal(list(1, 2), fn(a = list(1), c = list(2)))
    expect_equal(list(1, 2), fn(list(1), c = list(2)))
  })
  test_that("b can be missing in the opposite order IV", {
    expect_equal(list(1, 2), fn(c = list(2), a = list(1)))
  })
  test_that("a can be missing IV", {
    expect_equal(list(1, 2), fn(b = list(1), c = list(2)))
  })
  test_that("a can be missing in the opposite order IV", {
    expect_equal(list(1, 2), fn(c = list(2), b = list(1)))
  })
})

describe("missing arguments V", {
  fn <- ensure(pre = list(fn %is% "function", flag %is% logical),
    post = result %is% logical,
    function(fn, flag = TRUE) { fn(flag) })
  test_that("the function works V", {
    expect_true(fn(isTRUE, flag = TRUE))
    expect_false(fn(isTRUE, flag = FALSE))
    expect_true(fn(fn = isTRUE, flag = TRUE))
    expect_false(fn(fn = isTRUE, flag = FALSE))
  })
  test_that("the function works in the opposite order V", {
    expect_true(fn(flag = TRUE, fn = isTRUE))
    expect_false(fn(flag = FALSE, fn = isTRUE))
  })
  test_that("flag can be missing V", {
    expect_true(fn(isTRUE))
  })
})

describe("missing arguments VI", {
  fn <- ensure(pre = list(fn %is% "function", flag %is% logical, second_flag %is% character),
    post = result %is% logical,
    function(fn, flag = TRUE, second_flag = "hi") { fn(flag) })
  test_that("the function works VI", {
    expect_true(fn(fn = isTRUE, flag = TRUE, second_flag = "pizza"))
    expect_false(fn(fn = isTRUE, flag = FALSE, second_flag = "pizza"))
    expect_true(fn(isTRUE, flag = TRUE, second_flag = "pizza"))
    expect_false(fn(isTRUE, flag = FALSE, second_flag = "pizza"))
    expect_true(fn(isTRUE, TRUE, second_flag = "pizza"))
    expect_false(fn(isTRUE, FALSE, second_flag = "pizza"))
    expect_true(fn(isTRUE, TRUE, "pizza"))
    expect_false(fn(isTRUE, FALSE, "pizza"))
  })
  test_that("the function works in the opposite order VI", {
    expect_true(fn(fn = isTRUE, second_flag = "pizza", flag = TRUE))
    expect_false(fn(fn = isTRUE, second_flag = "pizza", flag = FALSE))
    expect_true(fn(isTRUE, second_flag = "pizza", flag = TRUE))
    expect_false(fn(isTRUE, second_flag = "pizza", flag = FALSE))
    expect_true(fn(flag = TRUE, fn = isTRUE, second_flag = "pizza"))
    expect_false(fn(flag = FALSE, fn = isTRUE, second_flag = "pizza"))
    expect_true(fn(flag = TRUE, fn = isTRUE, "pizza"))
    expect_false(fn(flag = FALSE, fn = isTRUE, "pizza"))
    expect_true(fn(flag = TRUE, second_flag = "pizza", fn = isTRUE))
    expect_false(fn(flag = FALSE, second_flag = "pizza", fn = isTRUE))
  })
  test_that("flag can be missing VI", {
    expect_true(fn(isTRUE, second_flag = "pizza"))
    expect_true(fn(fn = isTRUE, second_flag = "pizza"))
  })
  test_that("flag can be missing in the opposite order VI", {
    expect_true(fn(second_flag = "pizza", fn = isTRUE))
  })
  test_that("second_flag can be missing VI", {
    expect_false(fn(isTRUE, flag = FALSE))
    expect_false(fn(fn = isTRUE, flag = FALSE))
  })
  test_that("second_flag can be missing in the opposite order VI", {
    expect_false(fn(flag = FALSE, fn = isTRUE))
  })
})

describe("default arguments", {
  test_that("NULL can be a formal", {
    fn <- checkr::ensure(
      pre = list(x %is% numeric || is.null(x),
                 y %is% numeric || is.null(y)),
      function(x = NULL, y = NULL) list(x, y))
    expect_equal(list(NULL, NULL), fn())
  })
  test_that("empty string can be a formal", {
    fn <- checkr::ensure(
      pre = list(x %is% numeric || identical(x, ""),
                 y %is% numeric || identical(y, "")),
      function(x = "", y = "") list(x, y))
    expect_equal(list("", ""), fn())
  })
  test_that("NA can be a formal", {
    fn <- checkr::ensure(
      pre = list(x %is% numeric || is.na(x),
                 y %is% numeric || is.na(y)),
      function(x = NA, y = NA) list(x, y))
    expect_equal(list(NA, NA), fn())
  })
})

describe("finding formals", {
  test_that("finding a global variable", {
    fn <- checkr::ensure(pre = x %is% numeric, function(x) x)
    a <- 12
    expect_equal(12, fn(a))
    a <- "a"
    expect_error(fn(a), "x %is% numeric")
  })
  test_that("finding a constant", {
    fn <- checkr::ensure(pre = x %is% numeric, function(x) x)
    expect_equal(10, fn(CONSTANT_NUMBER))
  })
  test_that("finding a base function", {
    fn <- checkr::ensure(pre = x %is% "function", function(x) x)
    expect_is(c, "function")
    expect_equal(c, fn(c))
  })
  test_that("finding a function from another package", {
    fn <- checkr::ensure(pre = x %is% "function", function(x) x)
    expect_is(testthat::test_that, "function")
    expect_equal(testthat::test_that, fn(testthat::test_that))
  })
  test_that("it can find a function - complex example", {
    batch <- checkr::ensure(
      pre = list(batch_fn %is% "function",
        keys %is% atomic || keys %is% list,
        size %is% numeric, size > 0, length(size) == 1, size %% 1 == 0,
        combination_strategy %is% "function",
        trycatch %is% logical,
        retry %is% numeric, retry >= 0, retry %% 1 == 0),
      function(batch_fn, keys, size = 50, combination_strategy = c,
        trycatch = FALSE, retry = 0) {
          function(...) {
            list(result = combination_strategy(batch_fn(...)),
              size = size,
              trycatch = trycatch,
              retry = retry)
          }
      })
      expect_silent(fn <- batch(function(x) x + 1, "x", size = 100))
      expect_is(fn, "function")
      target <- list(result = seq(2, 11), size = 100, trycatch = FALSE, retry = 0)
      expect_equal(target, fn(seq(10)))
    })

  describe("threading I - numerics", {
    a <- 1
    fn <- checkr::ensure(pre = w %is% numeric, function(w) w + 1)
    fn2 <- checkr::ensure(pre = x %is% numeric, function(x) x + 2)
    fn3 <- checkr::ensure(pre = y %is% numeric, function(y) y + 3)
    fn4 <- checkr::ensure(pre = z %is% numeric, function(z) z + 4)
    test_that("threading one function up - numerics", {
      expect_equal(2, fn(a))
    })
    test_that("threading two functions up - numerics", {
      expect_equal(4, fn(fn2(a)))
    })
    test_that("threading three functions up - numerics", {
      expect_equal(7, fn(fn2(fn3(a))))
    })
    test_that("threading four functions up - numerics", {
      expect_equal(11, fn(fn2(fn3(fn4(a)))))
    })
  })
  describe("threading I - dataframes", {
    a <- iris
    fn <- checkr::ensure(pre = w %is% dataframe, function(w) w)
    fn2 <- checkr::ensure(pre = x %is% dataframe, function(x) x)
    fn3 <- checkr::ensure(pre = y %is% dataframe, function(y) y)
    fn4 <- checkr::ensure(pre = z %is% dataframe, function(z) z)
    test_that("threading one function up - dataframes", {
      expect_equal(iris, fn(a))
    })
    test_that("threading two functions up - dataframes", {
      expect_equal(iris, fn(fn2(a)))
    })
    test_that("threading three functions up - dataframes", {
      expect_equal(iris, fn(fn2(fn3(a))))
    })
    test_that("threading four functions up - dataframes", {
      expect_equal(iris, fn(fn2(fn3(a))))
    })
  })
  describe("threading II - numerics", {
    a <- 1
    b <- 1
    fn1 <- function(x, y) {
      fn2(x, y)
    }
    fn2 <- function(c, d) {
      c <- fn3(c)
      d <- fn3(d)
      fn4(c, d)
    }
    fn3 <- checkr::ensure(pre = z %is% numeric, function(z) z + 3)
    fn4 <- checkr::ensure(pre = list(n %is% numeric, m %is% numeric),
      function(n, m) n + 4 + m + 4)
    test_that("threading four functions up II - numerics", {
      expect_equal(16, fn1(a, b))
    })
  })
  describe("threading II - dataframes", {
    a <- iris
    b <- iris
    fn1 <- function(x, y) {
      fn2(x, y)
    }
    fn2 <- function(c, d) {
      c <- fn3(c)
      d <- fn3(d)
      fn4(c, d)
    }
    fn3 <- checkr::ensure(pre = w %is% dataframe, function(w) head(w))
    fn4 <- checkr::ensure(pre = list(n %is% dataframe, m %is% dataframe),
      function(n, m) rbind(n, m))
    test_that("threading four functions up II - dataframes", {
      expect_equal(rbind(head(iris), head(iris)), fn1(a, b))
    })
  })
  describe("threading III - dataframes", {
    a <- iris
    b <- iris
    fn1 <- function(c, d) {
      fn2(c, d)
    }
    fn2 <- function(n, m) {
      fn3(rbind(n, m), m)
    }
    fn3 <- function(o, p) {
      o <- fn4(o)
      p <- fn4(p)
      fn5(o, p)
    }
    fn4 <- checkr::ensure(pre = q %is% dataframe, function(q) head(q))
    fn5 <- checkr::ensure(pre = list(r %is% dataframe, s %is% dataframe),
      function(r, s) rbind(r, s))
    test_that("threading four functions up III - dataframes", {
      expect_equal(rbind(head(rbind(iris, iris)), head(iris)), fn1(a, b))
    })
  })
  test_that("threading - custom arguments", {
    all.equal.custom_data <- function(target, current, ...) {
      target <- sanitize_data_frame(target)
      current <- sanitize_data_frame(current)
      all.equal.default(target, current)
    }
    sanitize_data_frame <- checkr::ensure(
      pre = list(df %is% dataframe, all(dim(df) > 0)),
      function(df) {
        id_col <- which(names(iris3) == "id")
        if (length(id_col) > 0) {
          df <- df[order(df[[id_col]]), ]
        }
        df <- df[, vapply(df, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
        df <- lapply(df, function(x) if (is.atomic(x) && !is.character(x)) as.character(x) else x)
        data.frame(df, stringsAsFactors = FALSE)
      })
    iris2 <- iris
    iris2$id <- seq(NROW(iris2))
    iris3 <- iris2[sample(seq(NROW(iris2))), ]
    expect_false(isTRUE(all.equal(iris2, iris3)))
    expect_true(isTRUE(all.equal(sanitize_data_frame(iris2), sanitize_data_frame(iris3))))
    class(iris2) <- c("custom_data", "data.frame")
    class(iris3) <- c("custom_data", "data.frame")
    expect_true(isTRUE(all.equal.custom_data(iris2, iris3)))
    expect_true(isTRUE(all.equal(iris2, iris3)))
  })
})

describe("matching up multiple missing formals", {
  test_that("Simple example", {
    fn <- function(a = 1, b = 2, c = 3, flag = "add") {
      if (identical(flag, "add")) {
        a + b + c
      } else {
        a - b - c
      }
    }
    expect_silent(result <- fn(1, c = 2))
    expect_equal(5, result)
  })
  test_that("More complex example", {
    batch <- checkr::ensure(
      pre = list(batch_fn %is% "function",
        keys %is% atomic || keys %is% list,
        size %is% numeric, size > 0, length(size) == 1, size %% 1 == 0,
        trycatch %is% logical,
        retry %is% numeric, retry >= 0, retry %% 1 == 0),
      function(batch_fn, keys, size = 50, flag = "flag", trycatch = FALSE, retry = 0) {
          function(...) {
            list(result = batch_fn(...),
              size = size,
              flag = flag,
              trycatch = trycatch,
              retry = retry)
          }
      })
    expect_silent(fn <- batch(function(x) x + 1, "x", flag = "truck"))
    expect_is(fn, "function")
    target <- list(result = seq(2, 11), size = 50,
      flag = "truck", trycatch = FALSE, retry = 0)
    expect_equal(target, fn(seq(10)))
  })
})

test_that("lapply works", {
  add_one <- checkr::ensure(pre = x %is% numeric, function(x) x + 1) 
  expect_equal(seq(2, 6), lapply(seq(5), add_one))
})

describe("printing calculates preconditions, postconditions, and the before_fn", {
  called_pre <- FALSE
  called_post <- FALSE
  called_prevalid <- FALSE
  with_mock(
    `checkr::preconditions` = function(...) { called_pre <<- TRUE },
    `checkr::postconditions` = function(...) { called_post <<- TRUE },
    `checkr::get_prevalidated_fn` = function(...) { called_prevalid <<- TRUE }, {
      expect_false(called_pre)
      expect_false(called_post)
      expect_false(called_prevalid)
      print(random_string)
      expect_true(called_pre)
      expect_true(called_post)
      expect_true(called_prevalid)
    })
})
