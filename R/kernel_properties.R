
#' @export
#' @rdname kernelfun
#' 
#' @examples 
#' kernel_properties("gaussian")
#' 
kernel_properties <- 
function(name, 
         derivative = FALSE)
{
  name <- match.arg(tolower(name), .kernelsList())
  
  canonical_bandwidth <- switch(name, 
                                biweight = 5 * sqrt(7)/49, 
                                chernoff = NA_real_, 
                                cosine = 3/4 * sqrt(1/3 - 2/pi^2), 
                                eddy = NA_real_, 
                                epanechnikov = 3/(5 * sqrt(5)),
                                gaussian = 1/(2 * sqrt(pi)), 
                                optcosine = sqrt(1 - 8/pi^2) * pi^2/16, 
                                rectangular = sqrt(3)/6,
                                triangular = sqrt(6)/9, 
                                uniform = NA_real_)
  
  canonical_bandwidth_deriv <- NA_real_
  
  fac <- switch(name, 
                biweight = 2 * sqrt(7), 
                chernoff = NA, 
                cosine = 2/sqrt(1/3 - 2/pi^2), 
                eddy = NA, 
                epanechnikov = 2 * sqrt(5), 
                gaussian = 4, 
                optcosine = 2/sqrt(1 - 8/pi^2), 
                rectangular = 2 * sqrt(3), 
                triangular = 2 * sqrt(6), 
                uniform = NA)
  
  fac_deriv <- NA_real_
  
  integral_K <- switch(name, 
                       biweight = 1, 
                       chernoff = 1, 
                       cosine = 1, 
                       eddy = 1, 
                       epanechnikov = 1, 
                       gaussian = 1, 
                       optcosine = 1, 
                       rectangular = 1, 
                       triangular = 1, 
                       uniform = 1) 

  integral_K_deriv <- switch(name, 
                             biweight = NA, 
                             chernoff = 0, 
                             cosine = 0, 
                             eddy = 0, 
                             epanechnikov = 0, 
                             gaussian = 0, 
                             optcosine = 0, 
                             rectangular = 0, 
                             triangular = 0, 
                             uniform = 0) 
  
  integral_K2 <- switch(name, 
                        biweight = 1/2, 
                        chernoff = 1/2,
                        cosine = (3/4)*sqrt(1/3 - 2/pi^2), 
                        eddy = 1.25, 
                        epanechnikov = 3/5, 
                        gaussian = 1/(2*sqrt(pi)), 
                        optcosine = (pi^2/16)*sqrt(1 - 8/pi^2), 
                        rectangular = 1/2, 
                        triangular = 2/3, 
                        uniform = 1/2) 

  integral_K2_deriv <- switch(name, 
                              biweight = 15/(49*sqrt(7)), 
                              chernoff = 0,
                              cosine = (pi^2/4)*(sqrt(1/3 - 2/pi^2))^3, 
                              eddy = 9.375, 
                              epanechnikov = 3/2, 
                              gaussian = 0.1410474, 
                              optcosine = (pi^4/64)*(sqrt(1 - 8/pi^2))^3, 
                              rectangular = 0, 
                              triangular = 2, 
                              uniform = 0)
  
  continuity <- switch(name, 
                       biweight = Inf, 
                       chernoff = 0, 
                       cosine = Inf, 
                       eddy = 1, 
                       epanechnikov = 1, 
                       gaussian = Inf, 
                       optcosine = 1, 
                       rectangular = 0, 
                       triangular = 1, 
                       uniform = 0) 

  continuity_deriv <- switch(name, 
                             biweight = Inf, 
                             chernoff = Inf, 
                             cosine = Inf, 
                             eddy = 0, 
                             epanechnikov = 0, 
                             gaussian = Inf, 
                             optcosine = 0, 
                             rectangular = Inf, 
                             triangular = 0, 
                             uniform = Inf) 
  
  differentiability <- switch(name, 
                              biweight = Inf, 
                              chernoff = 0, 
                              cosine = Inf, 
                              eddy = 0,
                              epanechnikov = 0, 
                              gaussian = Inf, 
                              optcosine = 0, 
                              rectangular = 0, 
                              triangular = 0, 
                              uniform = 0) 
  
  differentiability_deriv <- switch(name, 
                                    biweight = Inf, 
                                    chernoff = Inf, 
                                    cosine = Inf, 
                                    eddy = 0,
                                    epanechnikov = 0, 
                                    gaussian = Inf, 
                                    optcosine = 0, 
                                    rectangular = Inf, 
                                    triangular = 0, 
                                    uniform = Inf) 
  
  if (derivative) {
    list(canonical_bandwidth = canonical_bandwidth_deriv, 
         continuity = continuity_deriv, 
         differentiability = differentiability_deriv,
         fac = fac_deriv, 
         integral_K = integral_K_deriv, 
         integral_K2 = integral_K2_deriv, 
         name = name, 
         derivative = derivative)
  } else {
    list(canonical_bandwidth = canonical_bandwidth, 
       continuity = continuity, 
       differentiability = differentiability,
       fac = fac, 
       integral_K = integral_K, 
       integral_K2 = integral_K2, 
       name = name, 
       derivative = derivative)
  }
}
