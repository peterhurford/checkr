#' Tests whether a string is simple.
#'
#' A simple string is an R object that is a length-1 vector of non-empty characters.
#' 
#' @param string character.
#' @examples
#'   is.simple_string("pizza")              # true
#'   is.simple_string(c("pizza", "apple"))  # false
#'   is.simple_string(iris)                 # false
#'   is.simple_string(NA)                   # false
#' @return a boolean whether or not string is simple string.
#' @export
is.simple_string <- function(string) {
  is.character(string) && length(string) == 1 && nzchar(string) && !is.na(string)
}
