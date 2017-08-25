#' Tests whether an argument to a function is present.
#'
#' This function is the opposite of missing.
#' @param ... list. The list of things to check for presence.
#' @export
present <- function(...) {
  try(get(deparse(substitute(...)),
    envir = parent.frame(), inherits = FALSE), silent = TRUE) %is_not% "try-error"
}
