context("Test distribution names")

test_that("'distr2name()' is ok", {
  expect_identical(distr2name(c("norm", "dnorm", "rhyper", "ppois", "toto")), 
                   c("Gaussian", "Gaussian", "Hypergeometric", "Poisson", "toto"))
})

test_that("'name2distr()' is ok", {
  expect_identical(name2distr(c("Cauchy", "Gaussian", "Generalized Extreme Value", "Toto")), 
                   c("cauchy", "norm", "gev", "toto"))
})
