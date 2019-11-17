#' @title 
#' Mid-range
#' 
#' @description 
#' Compute the mid-range of a numeric vector \code{x}, 
#' defined as the mean of the minimum and the maximum. 
#' 
#' @param x
#' numeric. A numeric vector. 
#' 
#' @param na_rm
#' logical. Should missing values be removed before computing the mid-range?
#' 
#' @return 
#' A numeric value, the mid-range.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Mid-range}.
#' 
#' @export
#' 
midrange <- function(x, na_rm = FALSE) {
  mean(range(x, na.rm = na_rm))
}
