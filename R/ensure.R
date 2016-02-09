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
  pre <- substitute(preconditions)
  post <- substitute(postconditions)
  force(fn)
  validated_fn <- function(...) {
    args <- list(...)
    if (is.empty(names(args))) { names(args) <- names(formals(fn)) }
    validate_(pre, env = args)
    args$result <- fn(...)
    validate_(post, env = args)
    args$result
  }
  class(validated_fn) <- append(class(fn), "validated_function")
  validated_fn
}


#' Get the stated preconditions of a validated function.
#' @param fn validated_function. The function to get the preconditions for.
#' @returns a call containing the preconditions.
preconditions <- function(fn) conditions_(fn, "pre")

#' Get the stated postconditions of a validated function.
#' @param fn validated_function. The function to get the postconditions for.
#' @returns a call containing the postconditions.
postconditions <- function(fn) conditions_(fn, "post")

conditions_ <- ensure(
  pre = list(fn %is% validated_function,
    key %in% c("pre", "post")),
  post = result %is% call,
  function(fn, key) { environment(fn)[[key]] })
