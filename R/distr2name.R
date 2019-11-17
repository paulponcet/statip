#' @title 
#' Conversion between abbreviated distribution names and proper names
#' 
#' @description 
#' The function \code{distr2name()} converts abbreviated 
#' distribution names to proper distribution names 
#' (e.g. \code{"norm"} becomes \code{"Gaussian"}). 
#' 
#' The function \code{name2distr()} does the reciprocal operation. 
#' 
#' @param x
#' character. A vector of abbreviated distribution names 
#' or proper distribution names. 
#' 
#' @return 
#' A character vector of the same length as \code{x}. 
#' Elements of \code{x} that are not recognized are kept unchanged 
#' (yet in lowercase). 
#' 
#' @export
#' 
#' @examples 
#' distr2name(c("norm", "dnorm", "rhyper", "ppois"))
#' name2distr(c("Cauchy", "Gaussian", "Generalized Extreme Value"))
#' 
distr2name <- function(x) {
  p <- c("", "d", "p", "q", "r") # prefix
  x[x %in% c(paste0(p, "bern"), "bernoulli")] <- "Bernoulli"
  x[x %in% c(paste0(p, "beta"), "beta")] <- "Beta"
  x[x %in% c(paste0(p, "binom"), "binomial")] <- "Binomial"
  x[x %in% c(paste0(p, "cauchy"), "cauchy")] <- "Cauchy"
  x[x %in% c(paste0(p, "chisq"), "chi-square")] <- "Chi-Square"
  x[x %in% c(paste0(p, "dagum"), "dagum")] <- "Dagum"
  x[x %in% c(paste0(p, "exp"), "exponential")] <- "Exponential"
  x[x %in% c(paste0(p, "f"), "f")] <- "F"
  x[x %in% c(paste0(p, "gamma"), "gamma")] <- "Gamma"
  x[x %in% c(paste0(p, "gh"), "gh")] <- "Generaralized Hyperbolic"
  x[x %in% c(paste0(p, "gev"), "gev")] <- "Generaralized Extreme Value"
  x[x %in% c(paste0(p, "gpd"), "gpd")] <- "Generalized Pareto"
  x[x %in% c(paste0(p, "geom"), "geometric")] <- "Geometric"
  x[x %in% c(paste0(p, "hyp"), "hyperbolic")] <- "Hyperbolic"
  x[x %in% c(paste0(p, "hyper"), "hypergeometric")] <- "Hypergeometric"
  x[x %in% c(paste0(p, "kumar"), "kumaraswamy")] <- "Kumaraswamy"
  x[x %in% c(paste0(p, "logis"), "logistic")] <- "Logistic"
  x[x %in% c(paste0(p, "lnorm"), "log-normal")] <- "Log-Normal"
  x[x %in% c(paste0(p, "multinom"), "multinomial")] <- "Multinomial"
  x[x %in% c(paste0(p, "nbinom"), "negative-binomial")] <- "Negative-Binomial"
  x[x %in% c(paste0(p, "nig"), "normal-inverse")] <- "Normal-Inverse"
  x[x %in% c(paste0(p, "norm"), "gaussian")] <- "Gaussian"
  x[x %in% c(paste0(p, "pois"), "poisson")] <- "Poisson"
  x[x %in% c(paste0(p, "t"), "student")] <- "Student"
  x[x %in% c(paste0(p, "tukey"), "tukey")] <- "Tukey"
  x[x %in% c(paste0(p, "unif"), "uniform")] <- "Uniform"
  x[x %in% c(paste0(p, "weibull"), "weibull")] <- "Weibull"
  x[x %in% c(paste0(p, "wilcox"), "wilcoxon")] <- "Wilcoxon"
  x
}
