context("Test 'erf()' function")

test_that("'erf()' is ok", {
  x <- rnorm(10)
  expect_identical(erf(c(x, NA)), c(2 * stats::pnorm(x * sqrt(2)) - 1, NA))
})
