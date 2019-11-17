#' @title 
#' Breakpoints to be passed to a Histogram
#' 
#' @description 
#' The function \code{find_breaks()} isolates a piece of code of 
#' the function \code{\link[MASS]{truehist}()} from package \pkg{MASS} 
#' that is used to compute the set of breakpoints to be applied for the 
#' construction of the histogram.
#' 
#' @param x
#' numeric. A vector.
#' 
#' @param nbins
#' integer or character. The suggested number of bins. 
#' Either a positive integer, or a character string naming a rule: 
#' \code{"Scott"} (the default) or \code{"Freedman-Diaconis"} or \code{"FD"}. 
#' (Case is ignored.)
#' 
#' @param h
#' numeric. The bin width, a strictly positive number 
#' (takes precedence over nbins).
#' 
#' @param x0
#' numeric. Shift for the bins - 
#' the breaks are at \code{x0 + h * (..., -1, 0, 1, ...)}.
#' 
#' @return 
#' A numeric vector.
#' 
#' @export
#' 
#' @seealso 
#' \code{\link[statip]{histo}()} in this package; 
#' \code{\link[MASS]{truehist}()} from package \pkg{MASS};
#' \code{\link[graphics]{hist}()} from package \pkg{graphics}.
#' 
find_breaks <- 
function(x, 
         nbins = "Scott", 
         h, 
         x0 = -h/1000)
{
  if (missing(h)) {
    if (is.character(nbins)) 
      nbins <- switch(casefold(nbins), 
                      scott = nclass_scott(x), 
                      `freedman-diaconis` = , 
                      fd = nclass_freedman_diaconis(x), 
                      sturges = nclass_sturges(x))
    if (!is.finite(nbins) || nbins <= 0) 
      stop("'nbins' must result in a positive integer", call. = FALSE)
    h <- diff(pretty(x, nbins))[1L]
  }
  if (!is.finite(h) || h <= 0) 
    stop("'h' must be strictly positive", call. = FALSE)
  first <- floor((min(x) - x0)/h)
  last <- ceiling((max(x) - x0)/h)
  breaks <- x0 + h * c(first:last)
    
  if (any(diff(breaks) <= 0))
    stop("'breaks' must be strictly increasing", call. = FALSE)
  if (min(x) < min(breaks) || max(x) > max(breaks)) 
    stop("'breaks' do not cover the range of 'x' values", call. = FALSE)
  
  breaks
}
