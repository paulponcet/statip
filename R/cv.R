#' @title 
#' Coefficient of variation
#' 
#' @description 
#' Compute the coefficient of variation of a numeric vector \code{x}, 
#' defined as the ratio between the standard deviation and the mean.
#' 
#' @param x
#' numeric. A numeric vector. 
#' 
#' @param na_rm
#' logical. Should missing values be removed before computing the coefficient of variation?
#' 
#' @param ...
#' Additional arguments to be passed to \code{\link{mean}()}.
#' 
#' @return 
#' A numeric value, the coefficient of variation.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Coefficient_of_variation}. 
#' 
#' @importFrom stats sd
#' @export
#' 
cv <- function(x, na_rm = FALSE, ...) {
  
  if (!is.null(list(...)$na.rm)) {
    stop("'na.rm' is not a valid argument, please use 'na_rm' instead", 
         call. = FALSE)
  }
  
  stats::sd(x, na.rm = na_rm) / mean(x, na.rm = na_rm, ...)
}
