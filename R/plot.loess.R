#' @title 
#' Basic plot of a loess object
#' 
#' @description 
#' Plots a loess object adjusted on one 
#' unique explanatory variable. 
#' 
#' @param x
#' An object of class \code{"loess"}. 
#' 
#' @param ...
#' Additional graphical arguments. 
#' 
#' @seealso 
#' \code{\link[stats]{loess}} from package \pkg{stats}. 
#' 
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#'
#' @examples 
#' reg <- loess(dist ~ speed, cars)
#' plot(reg)
#'
plot.loess <-
function(x, 
         ...)
{
  v <- x$x
  a <- order(v)
  graphics::plot(v, x$y, ...)
  graphics::lines(v[a], x$fitted[a], col = 2)
}


#' @importFrom graphics plot
#' @export
#' 
graphics::plot
