#' Tests whether an argument to a function is present.
#'
#' This function is the opposite of missing.
#' @export
present <- function(...) { !missing(...) }
