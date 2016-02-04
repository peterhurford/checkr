#' Ensure checks that certain preconditions and postconditions of a function are true.
#'
#' @param preconditions list. A list of preconditions to check.
#' @param postconditions list. A list of postconditions to check.
#' @param fn function. A function to run with validated pre- and postconditions.
#' @examples
#'   add <- ensure(pre = list(x %is% numeric, y %is% numeric),
#'     post = list(result %is% numeric),
#'     function(x, y) { x + y })
#' @return The original function, but with added validations.
#' @export
ensure <- function(preconditions, postconditions, fn) {
  pre <- substitute(preconditions)
  post <- substitute(postconditions)
  force(fn)
  function(...) {
    args <- list(...)
    if (is.empty(names(args))) { names(args) <- names(formals(fn)) }
    validate_(pre, env = args)
    args$result <- fn(...)
    validate_(post, env = args)
    args$result
  }
}
