#' @title 
#' Lag a vector
#' 
#' @description 
#' This function computes a lagged vector, shifting it back or forward.
#' 
#' @param x
#' A vector. 
#' 
#' @param k
#' integer. The number of lags. 
#' If \code{k < 0}, la serie est avancee au lieu d'etre retardee.
#' 
#' @param na
#' logical. If \code{na = TRUE} and \code{k > 0} 
#' (resp. \code{k < 0}), the \code{|k|} holes created in the lagged vector 
#' are put to \code{NA}; otherwise, the imputation depends on \code{cst}. 
#' 
#' @param cst
#' logical. 
#' If \code{na = FALSE} and \code{cst = TRUE}, the \code{|k|} holes 
#' created in the lagged vector are put to \code{x[[1L]]} 
#' (or to \code{x[[length(x)]]} if \code{k < 0}). 
#' If \code{na = FALSE} and \code{cst = FALSE}, 
#' these \code{|k|} holes are imputed by the \code{k} 
#' first values of \code{x} (or the \code{k} last values if \code{k < 0}).
#' 
#' @return 
#' A vector of the same type and length as \code{x}. 
#' 
#' @export
#' 
#' @examples 
#' v <- sample(1:10)
#' print(v)
#' lagk(v, 1)
#' lagk(v, 1, na = TRUE)
#' lagk(v, -2)
#' lagk(v, -3, na = TRUE)
#' lagk(v, -3, na = FALSE, cst = TRUE)
#' lagk(v, -3, na = FALSE)
#'
lagk <-
function (x, 
          k,
          na = FALSE,
          cst = FALSE)
{
  nx <- length(x)
  if(nx < abs(k)) {
    warning (paste("argument 'x' is too short to be lagged from ", k, " lags", 
                   sep = ""))
    if (na) {
      return(rep(NA,nx))
    } else {
      return(x)
    }
  } else {
    if (k > 0) {
      if (na) {
        deb <- rep(NA, k)
      } else {
        if (cst) {
          deb <- rep(x[1], k)
        } else {
          deb <- x[1:k]
        }
      }
      y <- c(deb, x[1:(nx - k)])
      names(y) <- names(x)
    }
    if (k < 0) {
      if (na) {
        fin <- rep(NA, (-k))
      } else {
        if (cst) {
          fin <- rep(x[length(x)], -k)
        } else {
          fin <- x[(nx+k+1):nx]
        }
      }
      y <- c(x[(-k + 1):nx], fin)
      names(y) <- names(x)
    }
    if (k == 0) {
      y <- x
    }
    return(y)
  }
}
