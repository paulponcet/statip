#' @title
#' Most frequent value(s)
#'
#' @description
#' The function \code{mfv} returns the most frequent value(s) (or mode(s))
#' found in a vector.
#' The function \code{mfv1} returns the first of these values, so that \code{mfv1(x)} 
#' is identical to \code{mfv(x)[[1L]]}. 
#' 
#' @details 
#' See David Smith' blog post 
#' \href{http://blog.revolutionanalytics.com/2016/07/understanding-na-in-r.html}{here} 
#' to understand the philosophy followed in the code of \code{mfv} for missing 
#' values treatment. 
#' 
#' @note 
#' \code{mfv} calls the function \code{\link{tabulate}}. 
#' 
#' @references 
#' \itemize{ 
#'   \item Dutta S. and Goswami A. (2010). 
#'   Mode estimation for discrete distributions. 
#'   \emph{Mathematical Methods of Statistics}, \bold{19}(4):374--384.
#' }
#' 
#' @param x
#' Vector of observations (of type numeric, integer, character, factor, or 
#' logical).
#' \code{x} is to come from a discrete distribution. 
#' 
#' @param na.rm 
#' logical. If \code{TRUE}, missing values do not interfer 
#' with the result, see 'Details'. 
#'
#' @return
#' The function \code{mfv} returns a vector of the same type as \code{x}.
#' One should be aware that this vector can be of length \code{> 1}, in case of multiple modes.
#' \code{mfv1} is safer in the sense that it always 
#' returns a vector of length \code{1} (the first of the modes found). 
#'
#' @export
#'
#' @examples
#' # Basic examples:
#' mfv(c(3, 3, 3, 2, 4))            # 3
#' mfv(c(TRUE, FALSE, TRUE))        # TRUE
#' mfv(c("a", "a", "b", "a", "d"))  # "a"
#'
#' mfv(c("a", "a", "b", "b", "d"))  # c("a", "b")
#' mfv1(c("a", "a", "b", "b", "d")) # "a"
#'
#' # With missing values: 
#' mfv(c(3, 3, 3, 2, NA))           # 3
#' mfv(c(3, 3, 2, NA))              # NA
#' mfv(c(3, 3, 2, NA), na.rm = TRUE)# 3
#' 
#' # With only missing values: 
#' mfv(c(NA, NA))                   # NA
#' mfv(c(NA, NA), na.rm = TRUE)     # NaN
#' 
mfv <-
function(x,
         na.rm = FALSE)
{
  cl <- class(x)[[1L]]
  n <- length(x)
  a <- sum(is.na(x))
  if (na.rm) n <- n - a
  if (n == 0L) return(NaN)
  
  f  <- factor(x)
  tf <- tabulate(f)
  n1 <- max(tf)
  
  if (!na.rm && a > 0L) {
    n2 <- ifelse(length(tf) > 1L, max(tf[tf!=n1]), 0L)
    if (n2 + a >= n1) return(as.vector(NA, mode = cl))
  }
  lf <- levels(f)[tf == n1]
  as.vector(lf, mode = cl)
}


#' @export
#' @rdname mfv
#'
mfv1 <-
function(x, 
         na.rm = FALSE)
{
  mfv(x, na.rm = na.rm)[[1L]]
}
