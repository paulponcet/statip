#' @title 
#' Tukey's trimean
#' 
#' @description 
#' Compute the trimean of a numeric vector \code{x}.
#' 
#' @param x
#' numeric. A numeric vector. 
#' 
#' @param na_rm
#' logical. Should missing values be removed before computing the trimean?
#' 
#' @param ...
#' Additional arguments to be passed to \code{\link[stats]{quantile}()}.
#' 
#' @return 
#' A numeric value, the trimean.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Trimean}
#' 
#' @importFrom stats median quantile
#' @export
#' 
trimean <- function(x, na_rm = FALSE, ...) {
  
  if (!is.null(list(...)$na.rm)) {
    stop("'na.rm' is not a valid argument, please use 'na_rm' instead", 
         call. = FALSE)
  }
  
  qs <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na_rm, ...)
  m <- stats::median(x, na.rm = na_rm)
  (qs[1L] + 2*m + qs[2L]) / 4
}
