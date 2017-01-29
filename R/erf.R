#' @title 
#' Error function
#' 
#' @description 
#' The function \code{erf} encodes the 
#' \href{https://en.wikipedia.org/wiki/Error_function}{error function}, 
#' defined as \code{erf(x) = 2 * F(x * sqrt(2)) - 1}, where 
#' \code{F} is the Gaussian distribution function. 
#' 
#' @param x
#' numeric. A vector of input values.
#' 
#' @param ...
#' additional arguments to be passed to \code{\link{pnorm}}. 
#' 
#' @return 
#' A numeric vector of the same length as \code{x}. 
#' 
#' @seealso 
#' \code{\link[stats]{pnorm}} from package \pkg{stats}. 
#' 
#' @importFrom stats pnorm
#' @export
#'  
erf <-
function(x,
         ...)
{
  return(2 * stats::pnorm(x * sqrt(2), ...) - 1)
}
