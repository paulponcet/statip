#' @title 
#' Kernel density estimation
#'
#' @description 
#' Return a function performing kernel density estimation. 
#' The difference between \code{\link[stats]{density}} and 
#' \code{densityfun} is similar to that between 
#' \code{\link[stats]{approx}} and \code{\link[stats]{approxfun}}. 
#'
#' @param x
#' numeric. The data from which the estimate is to be computed.
#' 
#' @param bw
#' numeric. The smoothing bandwidth to be used. 
#' See the eponymous argument of \code{\link[stats]{density}}. 
#' 
#' @param adjust
#' numeric. The bandwidth used is actually \code{adjust*bw}. 
#' This makes it easy to specify values like 'half the default' bandwidth.
#' 
#' @param kernel,window
#' character. A string giving the smoothing kernel to be used. 
#' Authorized kernels are listed in \code{\link[statip]{.kernelsList}()}. 
#' See also the eponymous argument of \code{\link[stats]{density}}. 
#' 
#' @param weights
#' numeric. A vector of non-negative observation weights, 
#' hence of same length as \code{x}. 
#' See the eponymous argument of \code{\link[stats]{density}}. 
#' 
#' @param width
#' this exists for compatibility with S; 
#' if given, and \code{bw} is not, 
#' will set \code{bw} to \code{width} 
#' if this is a character string, 
#' or to a kernel-dependent multiple of \code{width} if this is numeric.
#' 
#' @param n
#' The number of equally spaced points at which the density 
#' is to be estimated. 
#' See the eponymous argument of \code{\link[stats]{density}}. 
#' 
#' @param from,to
#' The left and right-most points of the grid at which the 
#' density is to be estimated; 
#' the defaults are \code{cut * bw} outside of \code{range(x)}. 
#' 
#' @param cut
#' By default, the values of \code{from} and \code{to} 
#' are cut bandwidths beyond the extremes of the data. 
#' This allows the estimated density to drop to 
#' approximately zero at the extremes.
#' 
#' @param na.rm
#' logical. If \code{TRUE}, missing values are removed 
#' from \code{x}. 
#' If \code{FALSE} any missing values cause an error.
#' 
#' @param ...
#' Additional arguments for (non-default) methods.
#' 
#' @return 
#' A function that can be called to generate a density. 
#'
#' @author 
#' Adapted from the \code{\link[stats]{density}} function of package \pkg{stats}. 
#' The C code of \code{BinDist} is copied from package \pkg{stats} and authored 
#' by the R Core Team with contributions from Adrian Baddeley. 
#' 
#' @seealso 
#' \code{\link[stats]{density}} and \code{\link[stats]{approxfun}} 
#' from package \pkg{stats}. 
#' 
#' @useDynLib statip, .registration=TRUE
#' @useDynLib statip BinDist
#' @importFrom stats approxfun
#' @importFrom stats fft
#' @importFrom stats bw.nrd0
#' @importFrom stats bw.nrd
#' @importFrom stats bw.ucv
#' @importFrom stats bw.bcv
#' @importFrom stats bw.SJ
#' @export
#' 
#' @examples 
#' x <- rlnorm(1000, 1, 1)
#' f <- densityfun(x, from = 0)
#' curve(f(x), xlim = c(0, 20))
#' 
densityfun <-
function(x,
         bw = "nrd0",
         adjust = 1,
         kernel = "gaussian",
         weights = NULL,
         window = kernel,
         width,
         n = 512,
         from,
         to,
         cut = 3,
         na.rm = FALSE,
         ...)
{
  if (!missing(...))
    warning("non-matched further arguments are disregarded")
  if (!missing(window) && missing(kernel))
    kernel <- window
  kernel <- match.arg(kernel, .kernelsList())
  #if (give.Rkern) return(kernel_properties(kernel)$canonical_bandwidth)
  if (!is.numeric(x))
    stop("argument 'x' must be numeric")
  x <- as.vector(x)
  x.na <- is.na(x)
  if (any(x.na)) {
    if (na.rm)
      x <- x[!x.na]
    else stop("'x' contains missing values")
  }
  N <- nx <- as.integer(length(x))
  if (is.na(N))
    stop("invalid value of length(x)")
  x.finite <- is.finite(x)
  if (any(!x.finite)) {
    x <- x[x.finite]
    nx <- length(x)
  }
  if (is.null(weights)) {
    weights <- rep.int(1/nx, nx)
    totMass <- nx/N
  } else {
    if (length(weights) != N)
      stop("'x' and 'weights' have unequal length")
    if (!all(is.finite(weights)))
      stop("'weights' must all be finite")
    if (any(weights < 0))
      stop("'weights' must not be negative")
    wsum <- sum(weights)
    if (any(!x.finite)) {
      weights <- weights[x.finite]
      totMass <- sum(weights)/wsum
    } else totMass <- 1
    if (!isTRUE(all.equal(1, wsum)))
      warning("sum(weights) != 1  -- will not get true density")
  }
  n.user <- n
  n <- max(n, 512)
  if (n > 512)
    n <- 2^ceiling(log2(n))
  if (missing(bw) && !missing(width)) {
    if (is.numeric(width)) {
      fac <- kernel_properties(kernel)$fac
      bw <- width/fac
    }
    if (is.character(width))
      bw <- width
  }
  if (is.character(bw))
    bw <- bandwidth(x, bw)
  if (!is.finite(bw))
    stop("non-finite 'bw'")
  bw <- adjust * bw
  if (bw <= 0)
    stop("'bw' is not positive")
  if (missing(from))
    from <- min(x) - cut * bw
  if (missing(to))
    to <- max(x) + cut * bw
  if (!is.finite(from))
    stop("non-finite 'from'")
  if (!is.finite(to))
    stop("non-finite 'to'")
  lo <- from - 4 * bw
  up <- to + 4 * bw
  y <- .Call(BinDist, x, weights, lo, up, n) * totMass
  kords <- seq.int(0, 2 * (up - lo), length.out = 2L * n)
  kords[(n + 2):(2 * n)] <- -kords[n:2]
  kords <- kernelfun(kernel)(kords/bw)/bw
  kords <- stats::fft(stats::fft(y) * Conj(stats::fft(kords)), inverse = TRUE)
  kords <- pmax.int(0, Re(kords)[1L:n]/length(y))
  xords <- seq.int(lo, up, length.out = n)
  stats::approxfun(xords, kords, method = "linear", 
                   yleft = 0, yright = 0, rule = 1)
}
