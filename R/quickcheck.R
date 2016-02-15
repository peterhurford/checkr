#' Create the necessary testing objects to quickcheck a function.
#' @param fn function. A function to generate test objects for.
#' @import validations
function_test_objects <- ensure(pre = fn %is% "function", post = result %is% list,
  function(fn) {
    formals <- names(formals(fn))
    testing_frame <- lapply(seq_along(formals), function(n) sample(test_objects()))
    names(testing_frame) <- formals
    if (fn %is% validated_function) {
      preconditions <- validations::preconditions(fn)
      pre_fn <- validations::get_prevalidated_fn(fn)
      formals <- names(formals(pre_fn))
      testing_frame <- lapply(testing_frame, function(set) {
        Filter(function(item) {
          env <- list(item)
          names(env) <- names(formals(pre_fn))
          validated <- try(validations::validate_(preconditions, env = env), silent = TRUE)
          !is(validated, "try-error")  # Turn validation error into TRUE/FALSE
        }, set) })
    }
    testing_frame
  })

#' Print function arguments
#' @examples
#' l <- list(x = seq(3), y = seq(4))
#' print_args(l)
#' [1] "x = 1:3, y = 1:4"
print_args <- function(x) {
  paste0(paste(names(x),
    unname(vapply(x, function(y) capture.output(dput(y)), character(1))),
  sep = " = "), collapse = ", ")
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
quickcheck <- ensure(pre = list(fn %is% "function", verbose %is% logical),
  post = isTRUE(result),
function(fn, postconditions = NULL, verbose = TRUE) {
  post <- substitute(postconditions)
  function_name <- deparse(substitute(fn))
  testing_frame <- function_test_objects(fn)
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

#TODO: Quickcheck function with more than one formal.
# Plan for this -- generate a different testing_frame using function_test_objects on each formal. Function will have to be refactored and renamed.

#TODO, but later: Can mix-in your own custom objects into the test objects
