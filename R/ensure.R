#' Ensure checks that certain preconditions and postconditions of a function are true.
#'
#' @param fn function. A function to run with validated pre- and postconditions.
#' @param preconditions list. A list of preconditions to check.
#' @param postconditions list. A list of postconditions to check.
#' @examples
#'   add <- ensure(pre = list(x %is% numeric, y %is% numeric),
#'     post = list(result %is% numeric),
#'     function(x, y) { x + y })
#' @return The original function, but also of class validated_function, with added validations.
#' @export
ensure <- function(fn, preconditions = list(), postconditions = list()) {
  if (is(fn, "validated_function")) {
    stop("The function has already been validated.")  
  }
  pre <- substitute(preconditions)
  post <- substitute(postconditions)
  force(fn)
  validated_fn <- function(...) {
    args <- list(...)
    formals <- names(formals(fn))

    # Goal here is to (a) impute names the user doesn't give with the formals
    # and (b) detect if any formals are missing so we can place in their defaults
    # or error
    missing_formals <- setdiff(formals, names(args))
    empty_names <- vapply(names(args), is.empty, logical(1))
    names(args)[empty_names] <- missing_formals[empty_names]

    # These formals are going to be the ones that need to be replaced with defaults
    # or error
    missing_formals <- setdiff(formals, names(args))

    # Get all the non-empty arguments to impute missing arguments.
    default_args <- Filter(Negate(is.name), formals(fn))
    for (pos in seq_along(default_args)) {
      if (!(names(default_args)[[pos]] %in% names(args))) {
        args[[names(default_args)[[pos]]]] <- default_args[[pos]]
      }
    }

    # Sometimes we have args that have a default of NULL that are missing.
    # This is difficult to populate, but alas.
    missing_defaults <- setdiff(names(default_args), names(args))
    if (length(missing_defaults) > 0) {
      length(args) <- length(args) + length(missing_defaults)
      names(args) <- Filter(Negate(is.empty), union(names(args), missing_defaults))
    }

    # Run the preconditions and postconditions.
    tryCatch(validate_(pre, env = args),
      error = function(e) {
        e <- as.character(e)
        flag <- "object '.*not found"
        if (grepl(flag, e)) {
          missing_args_error(gsub("' not found", "",
            gsub("object '", "", regmatches(e, regexpr(flag, e)))))
        } else { stop(e) } 
      })
    args$result <- fn(...)
    validate_(post, env = args)
    args$result
  }

  class(validated_fn) <- append(class(fn), "validated_function", 0)
  validated_fn
}


#' Get the stated preconditions of a validated function.
#' @param fn validated_function. The function to get the preconditions for.
#' @return a call containing the preconditions.
#' @export
preconditions <- function(fn) conditions_(fn, "pre")

#' Get the stated postconditions of a validated function.
#' @param fn validated_function. The function to get the postconditions for.
#' @return a call containing the postconditions.
#' @export
postconditions <- function(fn) conditions_(fn, "post")

conditions_ <- ensure(
  pre = list(fn %is% validated_function,
    key %in% c("pre", "post")),
  post = result %is% call,
  function(fn, key) { environment(fn)[[key]] })


#' Get the pre-validated function that is wrapped in validations.
#' @param fn validated_function. The function to get the pre-validated function for.
#' @return a call containing the postconditions.
#' @export
get_prevalidated_fn <- ensure(
  pre = fn %is% validated_function,
  post = list(result %is% "function", result %isnot% validated_function),
  function(fn) { environment(fn)$fn })


#' Print validated functions more clearly.
#' @param x function. The function to print.
#' @param ... Additional arguments to pass to print.
#' @export
print.validated_function <- function(x, ...) {
  print(list(
    preconditions = preconditions(x),
    postconditions = postconditions(x),
    fn = get_prevalidated_fn(x)),
  ...)
}


missing_args_error <- function(missing_args) {
  stop("Error on missing arguments: ",
    paste0(missing_args, collapse = ", "), call. = FALSE)
}
