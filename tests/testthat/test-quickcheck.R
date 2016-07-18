context("quickcheck")

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
      pre = list(length %is% numeric, length(length) == 1, length > 0, length < 1e+7,
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
  test_that("the preconditions can be passed explicitly using pre", {
    testing_frame <- function_test_objects(pre = x %is% list)[[1]]
    expect_true(testing_frame %contains_only% list)
  })
})

describe("custom testing frames", {
  test_that("it can be custom", {
    custom_testing_frame <- function_test_objects(identity, frame = list(x = 1))
    expect_equal(list(x = 1), custom_testing_frame)
  })
  test_that("a custom testing frame must match the formals of the function", {
    expect_error(function_test_objects(function(x) x, frame = list(y = 1)))
  })
})

describe("quickcheck", {
  describe("integration tests", {
    test_that("simple success example I", {
      quickcheck(identity)
    })
    test_that("simple seccess example II", {
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
      expect_false(quickcheck(add_one, testthat = FALSE))
    })
    test_that("simple failure example II", {
      add_one <- ensure(
        pre = x %is% character, # quickcheck will only generate characters which will fail
        post = result %is% numeric,
        function(x) x + 1)
      expect_false(quickcheck(add_one, testthat = FALSE))
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
      expect_false(quickcheck(ensure(
        pre = list(length %is% numeric, length(length) == 1, length > 0,
          alphabet %is% list || alphabet %is% vector,
            alphabet %contains_only% simple_string),
        post = list(nchar(result) == length, length(result) == 1,
          is.character(result), all(strsplit(result, "")[[1]] %in% alphabet)),
        random_string), testthat = FALSE))
    })
    test_that("random string example - success", {
      random_string <- function(length, alphabet) {
        paste0(sample(alphabet, length, replace = TRUE), collapse = "")
      }
      quickcheck(ensure(
        pre = list(length %is% numeric, length(length) == 1, length > 0, length < 1e7,
          alphabet %is% list || alphabet %is% vector,
          alphabet %contains_only% simple_string,
          all(sapply(alphabet, nchar) == 1)),
        post = list(nchar(result) == length, length(result) == 1,
          is.character(result), all(strsplit(result, "")[[1]] %in% alphabet)),
        random_string))
    })
  })
  describe("unit tests", {
    test_that("it works on a long function", {
      quickcheck(ensure(pre = x %is% numeric,
        function(x) { x + x + x + x + x + x + x + x + x }))
    })
    test_that("it errors if the testing frame is reduced to 0", {
      impossible_preconditions <- ensure(pre = list(x %is% character, x %isnot% character),
        identity)
      expect_error(quickcheck(impossible_preconditions), "impossible to satisfy")
    })
    test_that("it errors if it quickchecks a function with no formals", {
      expect_error(
        quickcheck(ensure(post = result %is% character, function() "Hi!")),
        "no arguments")
    })
    test_that("NULL is allowed", {
      quickcheck(ensure(post = result %is% NULL, function(x) NULL))
    })
    test_that("empty string is allowed", {
      quickcheck(ensure(post = identical(result, ""), function(x) ""))
    })
    test_that("NA is allowed", {
      quickcheck(ensure(post = result %is% NA, function(x) NA))
    })
    test_that("it can layer postconditions", {
      random_string <- ensure(
        pre = list(length %is% numeric, length(length) == 1, length > 0, length < 1e7,
          alphabet %is% list || alphabet %is% vector,
          alphabet %contains_only% simple_string,
          all(sapply(alphabet, nchar) == 1)),
        post = list(nchar(result) == length, length(result) == 1, is.character(result)),
        function(length, alphabet) {
          paste0(sample(alphabet, length, replace = TRUE), collapse = "")
        })
      quickcheck(post = list(nchar(result) == length,
        all(strsplit(result, "")[[1]] %in% alphabet)), random_string)
    })
  })
})

describe("print_args", {
  test_that("works on a simple example I", {
    expect_equal("a = \"a\"", print_args(list(a = "a")))
  })
  test_that("works on a simple example II", {
    expect_equal("x = 1:3, y = 1:4",
      print_args(list(x = seq(3), y = seq(4))))
  })
  test_that("works on a simple example III", {
    expect_equal("x = list(3), y = list(4)",
      print_args(list(x = list(3), y = list(4))))
  })
  test_that("works on a simple example IV", {
    expect_equal("x = list(3, 2, \"a\"), y = list(4, 3, \"b\")",
      print_args(list(x = list(3, 2, "a"), y = list(4, 3, "b"))))
  })
  test_that("works on a dataframe", {
    expect_equal(paste0("df = structure(list(a = 1, b = 2), ",
    ".Names = c(\"a\", \"b\"), row.names = c(NA, -1L), class = \"data.frame\")"),
      print_args(list(df = data.frame(a = 1, b = 2))))
  })
  test_that("works on a long list", {
    expect_equal(paste0("x = list(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 41",
      ", 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)"),
      print_args(list(x = list(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 41,
        2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))))
  })
})
