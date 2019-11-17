#' @title
#' Most frequent value(s)
#'
#' @description
#' The function \code{mfv()} returns the most frequent value(s) (or mode(s))
#' found in a vector.
#' The function \code{mfv1} returns the first of these values, so that 
#' \code{mfv1(x)} is identical to \code{mfv(x)[[1L]]}. 
#' 
#' @details 
#' See David Smith' blog post 
#' \href{http://blog.revolutionanalytics.com/2016/07/understanding-na-in-r.html}{here} 
#' to understand the philosophy followed in the code of \code{mfv} for missing 
#' values treatment. 
#' 
#' @note 
#' \code{mfv()} calls the function \code{\link[base]{tabulate}}. 
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
#' @param na_rm 
#' logical. If \code{TRUE}, missing values do not interfer 
#' with the result, see 'Details'. 
#' 
#' @param ...
#' Additional arguments (currently not used). 
#'
#' @return
#' The function \code{mfv} returns a vector of the same type as \code{x}.
#' One should be aware that this vector can be of length \code{> 1}, in case of 
#' multiple modes.
#' \code{mfv1} always returns a vector of length \code{1} 
#' (the first of the modes found). 
#'
#' @export
#'
#' @examples
#' # Basic examples:
#' mfv(integer(0))                      # NaN
#' mfv(c(3, 3, 3, 2, 4))                # 3
#' mfv(c(TRUE, FALSE, TRUE))            # TRUE
#' mfv(c("a", "a", "b", "a", "d"))      # "a"
#'
#' mfv(c("a", "a", "b", "b", "d"))      # c("a", "b")
#' mfv1(c("a", "a", "b", "b", "d"))     # "a"
#'
#' # With missing values: 
#' mfv(c(3, 3, 3, 2, NA))               # 3
#' mfv(c(3, 3, 2, NA))                  # NA
#' mfv(c(3, 3, 2, NA), na_rm = TRUE)    # 3
#' mfv(c(3, 3, 2, 2, NA))               # NA
#' mfv(c(3, 3, 2, 2, NA), na_rm = TRUE) # c(2, 3)
#' mfv1(c(3, 3, 2, 2, NA), na_rm = TRUE)# 2
#' 
#' # With only missing values: 
#' mfv(c(NA, NA))                   # NA
#' mfv(c(NA, NA), na_rm = TRUE)     # NaN
#' 
#' # With factors
#' mfv(factor(c("a", "b", "a")))
#' mfv(factor(c("a", "b", "a", NA)))
#' mfv(factor(c("a", "b", "a", NA)), na_rm = TRUE)
#' 
mfv <-
function(x,
         ...)
{
  UseMethod("mfv")
}


# TODO: ne marche pas avec , devrait renvoyer NA

#' @export
#' @rdname mfv
#' 
mfv.default <- 
function(x,
         na_rm = FALSE, 
         ...)
{
  mfv(tableNA(x), na_rm = na_rm, ...)
  
  # cl <- typeof(x) #class(x)[[1L]]
  # n <- length(x)
  # a <- sum(is.na(x))
  # if (na_rm) n <- n - a
  # if (n == 0L) return(NaN)
  # 
  # f  <- factor(x)
  # lf <- levels(f)
  # tf <- tabulate(f)
  # wf <- which.max(tf)
  # n1 <- tf[wf] #max(tf)
  # v  <- lf[tf == n1]
  # 
  # if (!na_rm && a > 0L) {
  #   n2 <- ifelse(length(tf) > 1L, max(tf[-wf]), 0L)
  #   if (n2 + a >= n1) v <- NA
  # }
  # 
  # if (is.factor(x)) {
  #   return(factor(v, levels = lf))
  # } else {
  #   return(as.vector(v, mode = cl))
  # }
}


#' @export
#' @rdname mfv
#' 
mfv.tableNA <- 
function(x, 
         na_rm = FALSE, 
         ...)
{
  l <- list(...)
  if (!is.null(l$na.rm)) {
    message("argument 'na.rm' is soft-deprecated, please start using 'na_rm' instead")
    na_rm <- l$na.rm
  }
  
  tf <- x
  
  a <- tf["<NA>"]
  if (na_rm || is.na(a)) {
    a <- 0L
  }
  tf["<NA>"] <- 0L
  
  if (length(tf) == 1L && a == 0L) return(NaN)

  wf <- which.max(tf)
  n1 <- tf[wf]
  v  <- names(tf)[tf == n1]
  
  if (length(tf) == 1L && !na_rm) {
    v <- NA
  }
    
  if (!na_rm && a > 0L) {
    n2 <- ifelse(length(tf) > 1L, max(tf[-wf]), 0L)
    if (n2 + a >= n1) v <- NA
  }
  
  if (attr(x, "is_factor_x")) {
    return(factor(v, levels = levels(x)))
  } else {
    return(as.vector(v, mode = attr(x, "type_of_x")))
  }
}


#' @export
#' @rdname mfv
#'
mfv1 <-
function(x, 
         na_rm = FALSE, 
         ...)
{
  mfv(x, na_rm = na_rm, ...)[[1L]]
}
