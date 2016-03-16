#' Validate checks that certain facts are true.
#'
#' @param ... list. A list of conditions to check.
#' @examples
#'   validate(1 == 1, "a" %is% character, length(c(1, 2, 3)) == 3)
#' @return Either TRUE or stops with a list of errors.
#' @export
validate <- function(...) {
  conditions <- substitute(list(...))
  validate_(conditions)
}

#' Validate without NSE.
#' @param conditions list. A list of conditions to check.
#' @param env environment. An optional environment to evaluate within. Defaults to
#' \code{parent.frame(2)}, which contains the variables in the scope immediately beyond
#' the validate (though not the validate_) function.
#' @export
validate_ <- function(conditions, env = parent.frame(2)) {
  # Substituted R expressions have length > 1, so we need to wrap them in lists.
  if (conditions[[1]] != substitute(list) && is.call(conditions)) {
    conditions <- list(conditions)
  }
  errors <- Filter(Negate(is.null), lapply(conditions, verify_condition, env = env))
  if (length(errors) > 0) {
    stop("Error on ", paste(errors, collapse = ", "), call. = FALSE)
  }
  TRUE
}

verify_condition <- function(condition, env) {
  if (identical(eval(condition, envir = env), FALSE)) { deparse(condition) }
  else { NULL }
}
