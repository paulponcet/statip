#' @title 
#' Alternative Table Creation
#' 
#' @description 
#' Count the occurrences of each factor level or value in a vector. 
#' 
#' @param x
#' numeric. An atomic vector or a factor. 
#' 
#' @return 
#' An object of class \code{"tableNA"}, which is the result of 
#' \code{\link{tabulate}()} with three attributes:
#' \itemize{
#'   \item \code{type_of_x}: the result of \code{typeof(x)};
#'   \item \code{is_factor_x}: the result of \code{is.factor(x)};
#'   \item \code{levels}: the result of \code{levels(x)}.
#' }
#' The number of missing values is always reported.
#' 
#' @export
#' 
#' @examples 
#' tableNA(c(1,2,2,1,3))
#' tableNA(c(1,2,2,1,3, NA))
#' 
tableNA <- function(x) {
  
  stopifnot(is.atomic(x))
  if (length(x) == 0L) {
    fx <- factor(x, exclude = NULL, levels = NA)
  } else {
    fx <- factor(x, exclude = NULL)
  }
  tab <- tabulate(fx)
  ns <- levels(fx)
  ns[is.na(ns)] <- "<NA>"
  names(tab) <- ns
  structure(tab, type_of_x = typeof(x), is_factor_x = is.factor(x), 
            levels = levels(x), class = "tableNA")
}
