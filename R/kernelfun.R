#' @title 
#' Smoothing kernels
#' 
#' @description 
#' The generic function \code{kernelfun} creates 
#' a smoothing kernel function. 
#' 
#' @param name
#' character. 
#' The name of the kernel to be used. 
#' Authorized kernels are listed in \code{\link{.kernelsList}()}. 
#' 
#' @param derivative
#' logical. If \code{TRUE}, the derivative of the kernel 
#' is returned. 
#' 
#' @param ...
#' Additional arguments to be passed to the kernel function. 
#' 
#' @return 
#' A function. 
#' 
#' @seealso 
#' \code{\link[stats]{density}} in package \pkg{stats}. 
#' 
#  #' @importFrom dplyr if_else
#' @export
#' 
#' @examples 
#' k <- kernelfun("epanechnikov")
#' curve(k(x), xlim = c(-1, 1))
#' 
kernelfun <-
function(name,
         ...)
{
  UseMethod("kernelfun")
}


#' @export
#' @rdname kernelfun
#' 
kernelfun.function <- 
function(name, 
         ...)
{
  x <- deparse(substitute(name))
  Kfun <- name
  attr(Kfun, "name") <- x
  Kfun
}


#' @export
#' @rdname kernelfun
#' 
kernelfun.character <-
function(name,
         derivative = FALSE,
         ...)
{
  name <- match.arg(tolower(name), .kernelsList())
  kname <- if (derivative) { 
    paste0(".kernel.d", name) 
  } else { 
      paste0(".kernel.", name) 
    }
  Kfun <- function(x) {
    do.call(kname, list(x, ...))
  }
  attr(Kfun, "name") <- name
  Kfun
}


.kernel.biweight <-
function(x,
         ...)
{
  a <- sqrt(7)
  ax <- abs(x)
  ifelse(ax < a, (15/16) * (1 - (ax/a)^2)^2/a, 0)
}


# Derivative
.kernel.dbiweight <-
function(x,
         ...)
{
  a <- sqrt(7)
  ax <- abs(x)
  ifelse(ax < a, -(15/4) * x * (1 - (ax/a)^2)/a^3, 0)
}


.kernel.uniform <- .kernel.chernoff <-
function(x,
         ...)
{
  ifelse(abs(x) <= 1, 1/2, 0)
}


# Derivative
.kernel.duniform <- .kernel.dchernoff <-
function(x,
         ...)
{
  0
}


.kernel.cosine <-
function(x,
         ...)
{
  a <- 1/sqrt(1/3 - 2/pi^2)
  ifelse(abs(x) < a, (1 + cos(pi*x/a))/(2*a), 0)
}


# Derivative                          
.kernel.dcosine <-
function(x,
         ...)
{
  a <- 1/sqrt(1/3 - 2/pi^2)
  ifelse(abs(x) < a, -(pi/(2*a^2))*sin(pi*x/a), 0)
}


.kernel.eddy <-
function(x,
         ...)
{
  #ax <- abs(x)
  ifelse(abs(x) <= 1, (15/32) * (3 - 10*x^2 + 7*x^4), 0)
}


# Derivative
.kernel.deddy <-
function(x,
         ...)
{
  ifelse(abs(x) <= 1, (15/32) * (-20*x + 28*x^3), 0)
}


.kernel.epanechnikov <-
function(x,
         ...)
{
  ifelse(abs(x) <= 1, (3/4) * (1 - x^2), 0)
}


# Derivative
.kernel.depanechnikov <-
function(x,
         ...)
{
  ifelse(abs(x) <= 1, (-3*x/2), 0)
}


.kernel.gaussian <-
function(x,
         ...)
{
  stats::dnorm(x)
}


# Derivative
.kernel.dgaussian <-
function(x,
         ...)
{
  -x*stats::dnorm(x)
}


.kernel.optcosine <-
function(x,
         ...)
{
  a <- 1/sqrt(1 - 8/pi^2)
  ifelse(abs(x) < a, (pi/4) * cos(pi * x/(2*a))/a, 0)
}


# Derivative
.kernel.doptcosine <-
function(x,
         ...)
{
  a <- 1/sqrt(1 - 8/pi^2)
  ifelse(abs(x) < a, -(pi^2/(8*a^2)) * sin(pi * x/(2*a)), 0)
}


.kernel.rectangular <-
function(x,
         ...)
{
  a <- sqrt(3)
  ifelse(abs(x) < a, 0.5/a, 0)
}


# Derivative
.kernel.drectangular <-
function(x,
         ...)
{
  0
}


.kernel.triangular <-
function(x,
         ...)
{
  ax <- abs(x)
  ifelse(ax <= 1, (1 - ax), 0)
}


# Derivative
.kernel.dtriangular <-
function(x,
         ...)
{
  ax <- abs(x)
  ifelse(ax <= 1, -sign(x), 0)
}
