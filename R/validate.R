#' Require certain things are true about your function call.
#'
#' @param param(s) type. Description of param 
#' @examples \dontrun{
#    # fill in with example. I.E.
#'   a <- rnorm(100)
#'   b <- a*.7 + rnorm(100)
#'   plot(a, b) 
#'   # If wrapped in /code{dontrun} than also sample of return object if possible
#' }
#' @return Return Object Description
#' @export
validate <- function(...) {
  conditions <- substitute(list(...))
  errors <- Filter(Negate(is.null), lapply(conditions, verify_condition))
  if (length(errors) > 0) {
    stop("Failed conditions: ", paste(errors, collapse = ", "), call. = FALSE)
  }
  TRUE
}

verify_condition <- function(condition) {
  if (identical(eval(condition, envir = parent.frame(3)), FALSE)) { deparse(condition) }
  else { NULL }
}
