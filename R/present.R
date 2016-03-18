#' Tests whether an argument to a function is present.
#'
#' This function is the opposite of missing.
#' @export
present <- function(...) {
  try(get(deparse(substitute(...)),
    envir = parent.frame(), inherits = FALSE), silent = TRUE) %isnot% "try-error"
}
