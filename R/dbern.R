#' @title 
#' The Bernoulli distribution
#' 
#' @description 
#' Density, distribution function, quantile function and 
#' random generation for the Bernoulli distribution. 
#' 
#' @param x 
#' numeric. Vector of quantiles.
#' 
#' @param q
#' numeric. Vector of quantiles.
#' 
#' @param p
#' numeric in \code{[0, 1]}. Vector of probabilities.
#' 
#' @param n
#' number of observations. 
#' If \code{length(n) > 1}, the length is taken to be the number required.
#' 
#' @param prob
#' Probability of success on each trial.
#' 
#' @param log
#' logical. If \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' 
#' @param log.p
#' logical. If \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' 
#' @param lower.tail
#' logical. If \code{TRUE} (default), 
#' probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
#' 
#' @seealso 
#' See the help page of the \code{\link{Binomial}} distribution. 
#' 
#' @export
#' 
dbern <-
function(x, 
         prob, 
         log = FALSE)
{
  res <- rep(0, length(x))
  res[x == 1] <- prob
  res[x == 0] <- 1-prob
  if (log) return(log(res))
  res
}


#' @importFrom stats qbinom
#' @export
#' @rdname dbern
#' 
qbern <-
function(p, 
         prob, 
         lower.tail = TRUE, 
         log.p =FALSE)
{
  stats::qbinom(p, size = 1, prob = prob, 
                lower.tail = lower.tail, log.p = log.p)
}


#' @importFrom stats pbinom
#' @export
#' @rdname dbern
#' 
pbern <-
function(q, 
         prob, 
         lower.tail = TRUE, 
         log.p = FALSE)
{
  stats::pbinom(q, size = 1, prob = prob, 
                lower.tail = lower.tail, log.p = log.p)
}


#' @export
#' @rdname dbern
#' 
rbern <-
function(n, 
         prob)
{
  sample(c(0,1), n, replace = TRUE, prob = c(1-prob, prob))
}
