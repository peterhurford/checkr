#' Checks if all exported functions in the package are checked using checkr.
#'
#' @param package character. The name of the package to check.
#' @param stop logical. If TRUE, errors and tells you which functions are
#'    not checked. If FALSE, the function will return FALSE.
#' @return either a logical or an error.
#' @export
package_exports_checked <- function(package, stop = TRUE) {
  exports <- getNamespaceExports(package)
  errors <- lapply(exports, function(export) {
    fn <- get(export, envir = getNamespace(package))
    if (checkr::should_be_checked(fn)) {
      checkr::is.validated_function(fn)
    } else { TRUE }})
  names(errors) <- exports
  errors <- Filter(function(x) identical(x, FALSE), errors)
  if (length(errors) > 0) {
    if (isTRUE(stop)) {
      stop("The following functions are not checked by checkr: ",
        paste0(names(errors), collapse = ", "))
    } else { return(FALSE) }
  }
  TRUE
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
