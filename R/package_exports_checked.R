#' Checks if all exported functions in the package are checked using checkr.
#'
#' @param package character. The name of the package to check.
#' @export
package_exports_checked <- function(package) {
  all(vapply(getNamespaceExports(package), function(export) {
    fn <- get(export, envir = getNamespace(package))
    if (checkr::should_be_checked(fn)) {
      checkr::is.validated_function(fn)
    } else { TRUE }
  }, logical(1)))
}

#' Determine whether a function ought to be checked with checkr.
#'
#' Functions should be checked as long as they have formals.
#' @param fn function. The function to check.
#' @export
should_be_checked <- function(fn) { length(formals(fn)) > 0 }

#' Determine whether a function is checked with checkr.
#'
#' @param fn function. The function to check.
is.validated_function <- function(fn) { methods::is(fn, "validated_function") }
