#' Tests whether an object is empty.
#'
#' Empty items are NULL, NA, or nothing (length 0).
#'
#' @param obj ANY. The object to test.
#' @examples
#'   is.empty(NULL)
#'   is.empty(NA)
#'   is.empty(list(NULL, NA))
#'   is.empty(list())
#'   is.empty(c())
#'   is.empty(data.frame())
#'   is.empty("")
#'   is.empty(data.frame())
#' @return a boolean whether or not the object is empty.
#' @export
is.empty <- function(obj) {
  if (methods::is(obj, "list")) {
    all(vapply(obj, is.empty, logical(1)))
  } else {
    suppressWarnings(is.na(obj) || is.null(obj) || NROW(obj) == 0 || identical(obj, ""))
  }
}

#' @rdname is.empty
#' @export
is_empty <- is.empty
