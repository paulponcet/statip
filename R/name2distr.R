
#' @export
#' @rdname distr2name
#' 
name2distr <- function(x) {
  x <- tolower(x)
  x <- chartr(" -", "__", x)
  x[x == "bernoulli"] <- "bern"
  x[x == "beta"] <- "beta"
  x[x == "binomial"] <- "binom"
  x[x == "cauchy"] <- "cauchy"
  x[x %in% c("chi_square", "chisquare", "chisquared")] <- "chisq"
  #x[x == "dagum"] <- "dagum"
  x[x == "exponential"] <- "exp"
  x[x == "fdist"] <- "f"
  x[x == "gammadist"] <- "gamma"
  x[x %in% c("generalised_hyperbolic", "generalized_hyperbolic")] <- "gh"
  x[x %in% c("generalised_extreme_value", "generalized_extreme_value")] <- "gev"
  x[x %in% c("generalised_pareto", "generalized_pareto")] <- "gpd"
  x[x == "geometric"] <- "geom"
  x[x == "hyperbolic"] <- "hyp"
  x[x %in% c("hypergeometric", "hypergeom")] <- "hyper"
  x[x == "kumaraswamy"] <- "kumar"
  x[x == "logistic"] <- "logis"
  x[x %in% c("lognormal", "loggaussian")] <- "lnorm"
  x[x == "multinomial"] <- "multinom"
  x[x == "negative_binomial"] <- "nbinom"
  x[x == "normal_inverse"] <- "nig"
  x[x %in% c("gaussian", "normal")] <- "norm"
  x[x == "poisson"] <- "pois"
  x[x == "student"] <- "t"
  x[x == "tukey"] <- "tukey"
  x[x == "uniform"] <- "unif"
  x[x == "weibull"] <- "weibull"
  #x[x == "symmetric_stable"] <- "symstb"
  x
}
