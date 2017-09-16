#' Define if a number is within a certain range.
#'
#' @param num numeric. The number to check.
#' @param range numeric. A vector with one number specifying the lower-bound and another
#'    number specifying the upper-bound.
#' @export
`%within%` <- ensure(
  pre = list(num %is% numeric,
    range %is% vector, length(range) == 2, range %contains_only% numeric),
  post = result %is% logical,
  from <- function(num, range) {
    num >= range[[1]] & num <= range[[2]]
  })
