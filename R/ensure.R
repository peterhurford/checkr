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

    # If a function has multiple args but some of them are missing, we just
    # cut out the missing ones.
    missing_args <- NULL
    length(formals) <- length(args)
    if (is.null(names(args)) || all(formals %in% names(args))) {
      names(args) <- formals
    } else if (!(length(names(args)) == length(formals))) {
      missing_args <- Filter(function(x) { !(x %in% names(args)) }, formals)
    }

    # Get all the non-empty arguments to impute missing arguments.
    default_args <- Filter(Negate(is.name), formals(fn))
    for (pos in seq_along(default_args)) {
      if (!(names(default_args)[[pos]] %in% names(args))) {
        args[[names(default_args)[[pos]]]] <- default_args[[pos]]
      }
    }

    # Make sure all the formals are present in the args.
    if (is.null(missing_args)) {
      missing_args <- names(formals(fn))[which(!(names(formals(fn)) %in% names(args)))]
      missing_args <- missing_args[which(!(missing_args %in% names(default_args)))]
    }
    if (length(missing_args) > 0) { missing_args_error(missing_args) }

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
