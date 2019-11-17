#' @title 
#' Hellinger distance
#' 
#' @description 
#' Estimate the 
#' \href{https://en.wikipedia.org/wiki/Hellinger_distance}{Hellinger distance} 
#' between two random samples whose underdyling distributions 
#' are continuous. 
#' 
#' @details 
#' Probability density functions are estimated with 
#' \code{\link[statip]{densityfun}}. 
#' Then numeric integration is performed with \code{\link[stats]{integrate}}. 
#' 
#' @param x
#' numeric. A vector giving the first sample. 
#' 
#' @param y
#' numeric. A vector giving the second sample. 
#' 
#' @param lower
#' numeric. Lower limit passed to \code{\link[stats]{integrate}}. 
#' 
#' @param upper
#' numeric. Upper limit passed to \code{\link[stats]{integrate}}. 
#' 
#' @param method
#' integer. If \code{method = 1}, the usual definition 
#' of the Hellinger distance is used; if \code{method = 2}, 
#' an alternative formula is used. 
#' 
#' @param ...
#' Additional parameters to be passed to \code{\link[statip]{densityfun}}. 
#' 
#' @return 
#' A numeric value, the Hellinger distance. 
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Hellinger_distance}.
#' 
#' @seealso 
#' \code{\link[distrEx]{HellingerDist}} in package \pkg{distrEx}.
#' 
#' 
#' @importFrom stats integrate
#' @export
#' 
#' @examples 
#' x <- rnorm(200, 0, 2)
#' y <- rnorm(1000, 10, 15)
#' hellinger(x, y, -Inf, Inf)
#' hellinger(x, y, -Inf, Inf, method = 2)
#' 
hellinger <-
function(x,
         y,
         lower = -Inf,
         upper = Inf,
         method = 1,
         ...)
{
  fx <- densityfun(x, ...)
  fy <- densityfun(y, ...)
  if (method == 1) {
    g <- function(z) (fx(z)^0.5 - fy(z)^0.5)^2
    h2 <- stats::integrate(g, lower, upper)$value/2
  } else if (method == 2) {
    g <- function(z) (fx(z)*fy(z))^0.5
    h2 <- 1 - stats::integrate(g, lower, upper)$value
  } else {
    stop("incorrect 'method' argument", call. = FALSE)
  }
  sqrt(h2)
}
