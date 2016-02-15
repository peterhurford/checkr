#' Create the necessary testing objects to quickcheck a function.
#' @param fn function. A function to generate test objects for.
#' @import validations
function_test_objects <- validations::ensure(pre = fn %is% "function", post = result %is% list,
  function(fn) {
    if (fn %is% validated_function) {
      preconditions <- validations::preconditions(fn)
      if (preconditions[[1]] != substitute(list) && is.call(preconditions)) {
          preconditions <- list(preconditions)
      }
      pre_fn <- validations::get_prevalidated_fn(fn)
      formals <- names(formals(pre_fn))
      testing_frame <- lapply(seq_along(formals), function(n) sample(test_objects()))
      testing_frame <- tryCatch(lapply(seq_along(testing_frame), function(pos) {
        # First we try calculating each input independently so that we can maximize
        # the number of test samples.
        frame <- testing_frame[[pos]]
        Filter(function(item) {
          env <- list(item)
          names(env) <- formals[[pos]]
          for (precondition in as.list(preconditions)) {
            if (!grepl(formals[[pos]], deparse(precondition), fixed = TRUE)) { next }
            if (!isTRUE(eval(precondition, env = env))) { return(FALSE) }
          }
          TRUE
        }, frame)
      }), error = function(e) {
        if (grepl("not found", as.character(e), fixed = TRUE)) {
          # If there was a not found error, we assume it was because of interdependent
          # preconditions, so we go to the backup of calculating the arguments jointly.
          lapply(lapply(testing_frame, function(frame) {
            lapply(seq_along(testing_frame[[1]]), function(pos) {
              env <- lapply(testing_frame, `[[`, pos)
              names(env) <- formals
              for (precondition in as.list(preconditions)) {
                if (!isTRUE(eval(precondition, env = env))) { return(NULL) }
              }
              frame[[pos]]
            })
          }), function(frame) { Filter(Negate(is.null), frame) })
        } else { stop(e) }
      })
    } else {
      formals <- names(formals(fn))
      testing_frame <- lapply(seq_along(formals), function(n) sample(test_objects()))
    }
    names(testing_frame) <- formals
    testing_frame
  })

#' Print function arguments
#' @examples
#' l <- list(x = seq(3), y = seq(4))
#' print_args(l)
#' [1] "x = 1:3, y = 1:4"
print_args <- function(x) {
  paste0(paste(names(x),
    unname(sapply(x, function(y) capture.output(dput(y)))),
  sep = " = "), collapse = ", ")
}


#' Get the name from a passed function, which may be a validated function or just a block.
#'
#' @param orig_function_name. A substituted call of the function.
function_name <- function(orig_function_name) {
  function_name <- if (identical(deparse(as.list(orig_function_name)[[1]]), "ensure")) {
    as.list(orig_function_name)[length(as.list(orig_function_name))][[1]]
  } else {
    orig_function_name
  }
  deparse(function_name)
}


#' Quickcheck a function.
#'
#' Tests a function with many automatically generated inputs, checking that stated
#' postconditions hold.
#'
#' If given a function of class \code{validated_function}, the pre- and post-conditions can
#' be automatically inferred by the definition of the function. The test objects used to
#' test the function will be screened ahead of time to ensure they meet the preconditions.
#'
#' @param fn function. A function to randomly check postconditions for.
#' @param postconditions. Optional postconditions to quickcheck for.
#' @param verbose logical. Whether or not to announce the success.
#' @return either TRUE if the function passed the quickcheck or a specific error.
#' @import validations
quickcheck <- validations::ensure(pre = list(fn %is% "function", verbose %is% logical),
  post = isTRUE(result),
function(fn, postconditions = NULL, verbose = TRUE) {
  post <- substitute(postconditions)
  testing_frame <- function_test_objects(fn)
  if (any(vapply(testing_frame, length, numeric(1)) == 0)) {
    stop("No quickcheck testing frame was generated. Make sure your preconditions aren't",
      " impossible to satisfy!")
  }
  function_name <- function_name(substitute(fn))
  for (pos in seq_along(testing_frame[[1]])) {
    args <- lapply(testing_frame, `[[`, pos)
    tryCatch({
      result <- do.call(fn, args)
      validations::validate_(post, env = list(result = result))
    }, error = function(e) {
      stop("Quickcheck for ", function_name, " failed on item #", pos, ": ",
        print_args(args), call. = FALSE)
    })
  }
  if (isTRUE(verbose)) {
    message("Quickcheck for ", function_name, " passed on ", pos, " random examples!")
  }
  expect_true(TRUE)
  TRUE
})
#TODO: Handle splats
#TODO, but later: Can mix-in your own custom objects into the test objects
