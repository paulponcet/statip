#' @title 
#' Midhinge
#' 
#' @description 
#' Compute the midhinge of a numeric vector \code{x}, 
#' defined as the average of the first and third quartiles. 
#' 
#' @param x
#' numeric. A numeric vector. 
#' 
#' @param na_rm
#' logical. Should missing values be removed before computing the midhinge?
#' 
#' @param ...
#' Additional arguments to be passed to \code{\link[stats]{quantile}()}.
#' 
#' @return 
#' A numeric value, the midhinge.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Midhinge}.
#' 
#' @importFrom stats quantile
#' @export
#' 
midhinge <- function(x, na_rm = FALSE, ...) {
  
  if (!is.null(list(...)$na.rm)) {
    stop("'na.rm' is not a valid argument, please use 'na_rm' instead", 
         call. = FALSE)
  }
  
  mean(stats::quantile(x, probs = c(0.25, 0.75), na.rm = na_rm, ...))
}
