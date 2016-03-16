#' Tests whether an argument to a function is present.
#'
#' This function is the opposite of missing.
#' @export
present <- function(...) {
  try(list(...), silent = TRUE) %isnot% "try-error"
}
