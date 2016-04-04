#' Pending!
#'
#' This function has no formals, so it won't be counted against checkr!
#' @export
pending <- function() { "Pending!" }


#' Pending identity.
#'
#' This function needs to be checked or else the test will fail.
#' @export
pending_identity <- ensure(pre = list(x %is% any), identity)
