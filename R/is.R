#' Test for class membership
#'
#' @param match_object. The object to test for class.
#' @param expected_class. The name of the expected class.
#' @examples
#'   1 %is% numeric
#'   1.0 %is% double
#'   1L %is% integer
#'   iris %is% dataframe
#'   c("a", "b", "c") %is% vector
#'   "pizza" %is% simple_string
#'   list(a = "pizza", b = "pie") %is% c("character", "list")
#' @return Boolean whether or not the match_object is the expected_class.
#' @export
`%is%` <- function(match_object, expected_class) {
  if (is.name(substitute(expected_class))) {
    expected_class <- deparse(substitute(expected_class))
  }
  match_object %is_% expected_class
}


`%is_%` <- function(match_object, expected_class) {
  if (length(expected_class) > 1) {
    return(all(vapply(expected_class, `%is%`,
      match_object = match_object, logical(1))))
  }

  if (identical(expected_class, NULL)) {
    expected_class <- "NULL"
  }
  if (identical(expected_class, NA)) {
    expected_class <- "NA"
  }
  if (identical(expected_class, "string")) {
    expected_class <- "character"
  }
  if (identical(expected_class, "dataframe")) {
    expected_class <- "data.frame"
  }

  if (identical(expected_class, "simple_string")) {
    return(is.simple_string(match_object))
  }
  if (identical(expected_class, "double")) {
    return(is.double(match_object))
  }
  if (identical(expected_class, "empty")) {
    return(is.empty(match_object))
  }
  if (identical(expected_class, "NA")) {
    return(!is.null(match_object) && is.na(match_object))
  }
  if (identical(expected_class, "vector")) {
    return(is.vector(match_object) && !is(match_object, "list"))
  }
  is(match_object, expected_class)
}

#' Test whether a match object is not a member of a particular class.
#' @rdname grapes-is-grapes
#' @export
`%isnot%` <- function(match_object, expected_class) {
  Negate(`%is%`)(match_object, expected_class)
}
