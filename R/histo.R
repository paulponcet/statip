#' @title 
#' Alternative Histograms 
#' 
#' @description 
#' A simplified version of 
#' \code{\link[graphics]{hist}()} from package \pkg{graphics}.
#' 
#' @param x
#' numeric. A vector.
#' 
#' @param breaks
#' numeric. A vector of breakpoints to build the histogram, 
#' possibly given by \code{\link[statip]{find_breaks}()}.
#' 
#' @param ...
#' Additional parameters (currently not used).
#' 
#' @return 
#' An object of class \code{"histogram"}, which can be plotted 
#' by \code{\link[graphics]{plot.histogram}} from package \pkg{graphics}.
#' This object is a list with components:
#' \itemize{
#'   \item \code{breaks}: the \code{n+1} cell boundaries;
#'   \item \code{counts}: \code{n} integers giving the number of \code{x} 
#'   inside each cell;
#'   \item \code{xname}: a string with the actual \code{x} argument name.
#' }
#' 
#' @export
#' 
#' @seealso 
#' \code{\link[statip]{find_breaks}()} in this package; 
#' \code{\link[MASS]{truehist}()} from package \pkg{MASS};
#' \code{\link[graphics]{hist}()} from package \pkg{graphics}.
#' 
histo <- 
function(x, 
         breaks, 
         ...)
{
  stopifnot(is.numeric(x))
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  
  x <- x[is.finite(x)]
  
  bin <- cut(x, breaks, include.lowest = TRUE)
  counts <- tabulate(bin, length(levels(bin)))
  #dens <- counts/(diff(counts) * length(x))
  structure(list(breaks = breaks, counts = counts, xname = xname), 
            class = "histogram")
  #stats::stepfun(h$breaks, c(0, h$counts, 0), ...)
}


# mean.histogram <- function(x, ...) {
#   
# }
# 
# median.histogram <- function(x, ...) {
#   
# }

# min.histogram <- function(..., na.rm = FALSE) {
#   l <- list(...)
#   min(l[[1L]]$breaks, na.rm = na.rm)
# }
# 
# max.histogram <- function(..., na.rm = FALSE) {
#   l <- list(...)
#   max(l[[1L]]$breaks, na.rm = na.rm)
# }
