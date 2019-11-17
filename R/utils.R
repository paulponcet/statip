
nclass_sturges <- function(x) {
  ceiling(log2(length(x)) + 1)
} 


#' @importFrom stats var
#' 
nclass_scott <- function(x) {
  h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1/3)
  if (h > 0) 
    ceiling(diff(range(x))/h)
  else 1L
}


#' @importFrom stats IQR mad
#' 
nclass_freedman_diaconis <- function(x) {
  h <- stats::IQR(x)
  if (h == 0) 
    h <- stats::mad(x, constant = 2)
  if (h > 0) 
    ceiling(diff(range(x))/(2 * h * length(x)^(-1/3)))
  else 1L
}


# #' @importFrom stats formula predict 
# #' 
# as_fun <- function(x, ...) {
#   
#   ## Name of the X variables 
#   ..x <- x
#   rm(x)
#   ..n <- stats::formula(..x)
#   ..n <- all.vars(..n)[-1L]
#   if ("..x" %in% ..n) {
#     stop("the model's formula contains a variable called '..x', 
#        'as_fun()' does not work in this specific case")
#   }
#   if ("..n" %in% ..n) { 
#     stop("the model's formula contains a variable called '..n', 
#        'as_fun()' does not work in this specific case")
#   }
#   
#   ## Creation of the function to be returned, with no arguments yet
#   f <- function() {
#     df <- as.data.frame(as.list(environment()))
#     names(df) <- ..n
#     p <- stats::predict(..x, newdata = df, type = "vector", ...)
#     if (is.list(p)) {
#       if (!is.null(p$fit)) {
#         y <- p$fit
#       } else if (!is.null(p$pred)) {
#         y <- p$pred
#       } else {
#         stop("cannot find predicted values")
#       }
#     } else {
#       y <- p
#     }
#     unname(y)
#   }
#   
#   ## 'l' is the list used to name the arguments of the function 'f()'
#   l <- replicate(length(..n), substitute())
#   names(l) <- ..n
#   formals(f) <- l
#   f
# }
