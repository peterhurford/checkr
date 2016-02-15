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
quickcheck <- validations::ensure(
  pre = list(fn %is% "function", verbose %is% logical),
  #post = result %is% TRUE,
  function(fn, postconditions = NULL, verbose = TRUE) {
    function_name <- deparse(substitute(fn))
    testing_frame <- test_objects()
    if (fn %is% validated_function) {
      preconditions <- validations::preconditions(fn)
      testing_frame <- Filter(function(item) {
        env <- list(item)
        pre_fn <- validations::get_prevalidated_fn(fn)
        names(env) <- names(formals(pre_fn))
        validated <- try(validations::validate_(preconditions, env = env), silent = TRUE)
        !is(validated, "try-error")  # Turn validation error into TRUE/FALSE
      }, testing_frame)
    }
    for (pos in seq_along(testing_frame)) {
      item <- testing_frame[[pos]]
      tryCatch({
        result <- fn(item)
        if (!is.null(postconditions)) {
          validations::validate_(postconditions, env = list(result = result))
        }
      }, error = function(e) {
        stop("Quickcheck for ", function_name, " failed on item #", pos, ": ", dput(item))
      })
    }
    if (isTRUE(verbose)) {
      message("Quickcheck for ", function_name, " passed on ", pos, " random examples!")
    }
    expect_true(TRUE)
    TRUE
  })
#TODO: Test coverage for quickcheck.
#TODO: Quickcheck function with more than one formal.
#TODO: Can mix-in your own custom objects into the test objects
