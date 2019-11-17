#' @title 
#' Piecewise-constant regression
#' 
#' @description 
#' \code{picor} looks for a piecewise-constant function as a regression 
#' function. The regression is necessarily univariate. 
#' This is essentially a wrapper for \code{\link[rpart]{rpart}} (regression 
#' tree) and \code{\link[stats]{isoreg}}. 
#' 
#' @param formula
#' formula of the model to be fitted. 
#' 
#' @param data
#' optional data frame. 
#' 
#' @param method
#' character. If \code{method = "isotonic"}, then isotonic regression is 
#' applied with the \code{\link[stats]{isoreg}} from package \pkg{stats}. 
#' Otherwise, \code{\link[rpart]{rpart}} is used, with the corresponding 
#' \code{method} argument. 
#' 
#' @param min_length
#' integer. 
#' The minimal distance between two consecutive knots. 
#' 
#' @param ...
#' Additional arguments to be passed to \code{\link[rpart]{rpart}}. 
#' 
#' @param object,x,Fn
#' An object of class \code{"picor"}. 
#' 
#' @param newdata
#' data.frame to be passed to the \code{predict} method. 
#' 
#' @return 
#' An object of class \code{"picor"}, which is a list composed of the 
#' following elements: 
#' \itemize{
#'   \item formula: the formula passed as an argument; 
#'   \item x: the numeric vector of predictors; 
#'   \item y: the numeric vector of responses; 
#'   \item knots: a numeric vector (possibly of length 0), the knots found; 
#'   \item values: a numeric vector (of length \code{length(knots)+1}), 
#'   the constant values taken by the regression function between the knots. 
#' }
#' 
#' @importFrom rpart rpart
#' @importFrom stats isoreg knots model.frame
#' @export
#' 
#' @examples 
#' \dontrun{
#' s <- stats::stepfun(c(-1,0,1), c(1., 2., 4., 3.))
#' x <- stats::rnorm(1000)
#' y <- s(x)
#' p <- picor(y ~ x, data.frame(x = x, y = y))
#' print(p)
#' plot(p)
#' }
#' 
picor <- 
function(formula, 
         data, 
         method, 
         min_length = 0,
         ...)
{
  df <- stats::model.frame(formula, data)
  if (ncol(df) == 1L) {
    x <- seq_len(nrow(df))
  } else if (ncol(df) != 2L) {
    stop("incorrect formula", call. = FALSE)
  } else {
    x <- df[[2L]]
  }
  stopifnot(is.numeric(x))
  y <- df[[1L]]
  stopifnot(is.numeric(y))
  
  if (!missing(method) && method == "isotonic") {
    r <- stats::isoreg(x = x, y = y)
    r <- as.stepfun(r)
    knots <- stats::knots(r)
    values <- unname(r(knots))

  } else {
    ## Use -x instead of x to make the resulting prediction function 
    ## left-continuous instead of right-continuous
    r <- rpart::rpart(y ~ I(-x), na.action = rpart::na.rpart, method = method,
                      model = FALSE, x = FALSE, y = FALSE, ...)
    if (is.null(r$splits)) {
      knots <- max(x, na.rm = TRUE)
    } else {
      knots <- c(sort(-r$splits[,"index"]), max(x, na.rm = TRUE))
    }
    #r <- as_fun(r)
    values <- stats::predict(r, data.frame(x = knots))
  }
  
  z <- structure(list(formula = formula, 
                      x = x, 
                      y = y,
                      #predict = r, 
                      knots = unname(knots)[-length(knots)], 
                      values = values),#unname(r(knots))), 
                 class = "picor")
  prune(z, min_length = min_length)
}


#' @importFrom stats knots
#' @export
#' 
stats::knots


#' @export
#' @rdname picor
#' 
knots.picor <- 
function(Fn, ...)
{
  Fn$knots  
}


#' @importFrom stats predict
#' @export
#' 
stats::predict


#' @importFrom stats stepfun
#' @export
#' @rdname picor
#' 
predict.picor <- 
function(object, 
         newdata, 
         ...)
{
  if (is.data.frame(newdata)) {
    x <- all.vars(object$formula)[-1L]
    newdata <- newdata[[x]]
  }
  if (!is.numeric(newdata)) {
    stop("incorrect 'newdata' argument", call. = FALSE)
  }
  if (length(object$knots) == 0L) {
    f <- function(v) { rep(object$values, length(v)) }
  } else {
    f <- stats::stepfun(object$knots, object$values, right = TRUE)
  }
  f(newdata)
}


#' @importFrom graphics plot lines
#' @export
#' @rdname picor
#' 
plot.picor <- 
function(x, 
         ...)
{
  obj <- x
  v <- all.vars(obj$formula)
  x <- obj$x
  a <- order(x)
  graphics::plot(x, obj$y, xlab = v[2L], ylab = v[1L], ...)
  graphics::lines(x[a], predict(obj, x)[a], col = 2)
}


#' @export
#' @rdname picor
#' 
print.picor <- 
function(x, 
         ...)
{
  print(x[c("knots", "values")])
}
