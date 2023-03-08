#' Convert Array to NA if Empty
#'
#' @description \code{empty_to_na()} converts the received array to single value
#'   of \code{NA} if it is an empty array. Otherwise, the received array is
#'   returned as is.
#'
#' @param x an array
#'
#' @return
#' \code{NA} if x was empty. Otherwise, x is returned as is.
#'
#' @examples
#' empty_to_na(character(0))
#' empty_to_na(c())
#' empty_to_na(c(1,2,3))
#' empty_to_na(c('a','b','c'))
empty_to_na <- function(x) {
    if (length(x) == 0) NA else x
}
