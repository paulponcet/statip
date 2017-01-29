#' @title 
#' Conversion between abbreviated distribution names and proper names
#' 
#' @description 
#' The function \code{distr2name} converts abbreviated 
#' distribution names to proper distribution names 
#' (e.g. \code{"norm"} becomes \code{"Gaussian"}). 
#' 
#' The function \code{name2distr} does the reciprocal operation. 
#' 
#' @param x
#' character. A vector of abbreviated distribution names 
#' or proper distribution names. 
#' 
#' @return 
#' A character vector of the same length as \code{x}. 
#' Elements of \code{x} that are not recognized are kept unchanged. 
#' 
#' @export
#' 
#' @examples 
#' distr2name(c("norm", "dnorm", "rhyper", "ppois"))
#' name2distr(c("Cauchy", "Gaussian", "Generalized Extreme Value"))
#' 
distr2name <- 
function(x)
{
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "bern"), "bernoulli")] <- "Bernoulli"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "beta"), "beta")] <- "Beta"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "binom"), "binomial")] <- "Binomial"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "cauchy"), "cauchy")] <- "Cauchy"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "chisq"), "chi-square")] <- "Chi-Square"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "dagum"), "dagum")] <- "Dagum"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "exp"), "exponential")] <- "Exponential"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "f"), "f")] <- "F"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "gamma"), "gamma")] <- "Gamma"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "gh"), "gh")] <- "Generaralized Hyperbolic"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "gev"), "gev")] <- "Generaralized Extreme Value"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "gpd"), "gpd")] <- "Generalized Pareto"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "geom"), "geometric")] <- "Geometric"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "hyp"), "hyperbolic")] <- "Hyperbolic"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "hyper"), "hypergeometric")] <- "Hypergeometric"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "kumar"), "kumaraswamy")] <- "Kumaraswamy"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "logis"), "logistic")] <- "Logistic"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "lnorm"), "log-normal")] <- "Log-Normal"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "multinom"), "multinomial")] <- "Multinomial"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "nbinom"), "negative-binomial")] <- "Negative-Binomial"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "nig"), "normal-inverse")] <- "Normal-Inverse"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "norm"), "gaussian")] <- "Gaussian"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "pois"), "poisson")] <- "Poisson"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "t"), "student")] <- "Student"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "tukey"), "tukey")] <- "Tukey"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "unif"), "uniform")] <- "Uniform"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "weibull"), "weibull")] <- "Weibull"
  x[x %in% c(paste0(c("", "d", "p", "q", "r"), "wilcox"), "wilcoxon")] <- "Wilcoxon"
  x
}
