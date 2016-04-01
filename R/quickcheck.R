get_testing_frame <- function(formals, frame) {
  if (is.null(frame)) {
    lapply(seq_along(formals), function(n) sample(checkr:::test_objects()))
  } else {
    if (!identical(formals, names(frame))) {
      stop("The custom testing_frame you submitted does not match the formals.")
    } else {
      frame
    }
  }
}


#' Create the necessary testing objects to quickcheck a function.
#' @param fn function. A function to generate test objects for.
#' @param frame list. A custom testing_frame to use, if necessary.
function_test_objects <- function(fn, frame = NULL) {
  if (is(fn, "validated_function")) {
    preconditions <- preconditions(fn)
    if (preconditions[[1]] != substitute(list) && is.call(preconditions)) {
        preconditions <- list(preconditions)
    }
    if (length(preconditions) > 1) { preconditions <- preconditions[-1] }
    pre_fn <- checkr::get_prevalidated_fn(fn)
    formals <- names(formals(pre_fn))
    if (length(formals) == 0) {
      stop("You cannot quickcheck a function with no arguments.")
    }
    testing_frame <- get_testing_frame(formals, frame)
    testing_frame <- tryCatch(lapply(seq_along(testing_frame), function(pos) {
      # First we try calculating each input independently so that we can maximize
      # the number of test samples.
      frame <- testing_frame[[pos]]
      Filter(function(item) {
        env <- list(item)
        names(env) <- formals[[pos]]
        for (precondition in as.list(preconditions)) {
          if (!grepl(formals[[pos]], deparse(precondition), fixed = TRUE)) { next }
          if (!isTRUE(eval(precondition, envir = env))) { return(FALSE) }
        }
        TRUE
      }, frame)
    }), error = function(e) {
      # If there was an error, we assume it was because of interdependent
      # preconditions, so we go to the backup of calculating the arguments jointly.
      lapply(lapply(testing_frame, function(frame) {
        lapply(seq_along(testing_frame[[1]]), function(pos) {
          env <- lapply(testing_frame, `[[`, pos)
          names(env) <- formals
          for (precondition in as.list(preconditions)) {
            if (!isTRUE(eval(precondition, envir = env))) { return(NULL) }
          }
          frame[[pos]]
        }) }), function(frame) { Filter(Negate(is.null), frame) })
    })
  } else {
    formals <- names(formals(fn))
    testing_frame <- get_testing_frame(formals, frame)
  }
  names(testing_frame) <- formals
  testing_frame
}

#' Print function arguments
#' @param x ANY. The object to print args for.
#' @examples
#' l <- list(x = seq(3), y = seq(4))
#' checkr:::print_args(l)
print_args <- function(x) {
  paste0(paste(names(x),
    unname(sapply(x, function(y) {
      # Correct for the tendency of capture.output to go over one string.
      gsub("    ", "", paste0(capture.output(dput(y)), collapse = ""))
    })), sep = " = "), collapse = ", ")
}


#' Get the name from a passed function, which may be a validated function or just a block.
#'
#' @param orig_function_name call. A substituted call of the function.
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
#' @param postconditions list. Optional postconditions to quickcheck for.
#' @param verbose logical. Whether or not to announce the success.
#' @param testthat logical. Whether or not to run testthat.
#' @param frame list. A custom testing_frame to use, if necessary.
#' @return either TRUE if the function passed the quickcheck or FALSE if it didn't.
#' @export
quickcheck <- function(fn, postconditions = NULL, verbose = TRUE, testthat = TRUE,
  frame = NULL) {
  post <- substitute(postconditions)
  testing_frame <- checkr:::function_test_objects(fn, frame = frame)
  if (any(vapply(testing_frame, length, numeric(1)) == 0)) {
    stop("No quickcheck testing frame was generated. Make sure your preconditions aren't",
      " impossible to satisfy!")
  }
  function_name <- function_name(substitute(fn))
  failed <- FALSE
  for (pos in seq_along(testing_frame[[1]])) {
    if (identical(failed, FALSE)) {
      args <- lapply(testing_frame, `[[`, pos)
      tryCatch({
        result <- do.call(fn, args)
        checkr:::validate_(post, env = list(result = result))
      }, error = function(e) {
        failed <<- TRUE
      })
    }
  }
  if (identical(failed, FALSE)) {
    if (isTRUE(verbose)) {
      message("Quickcheck for ", function_name, " passed on ", pos, " random examples!")
    }
    if (isTRUE(testthat)) { testthat::expect_true(TRUE) }
    TRUE
  } else {
    error_msg <- paste0("Quickcheck for ", function_name, " failed on item #", pos, ": ",
      print_args(args))
    if (isTRUE(verbose) && !isTRUE(testthat)) { message(error_msg) }
    if (isTRUE(testthat)) { testthat::expect_true(FALSE, error_msg) }
    FALSE
  }
}
#TODO: Handle splats
#TODO, but later: Can mix-in your own custom objects into the test objects
