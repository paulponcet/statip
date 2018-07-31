
#' @importFrom stats as.stepfun
#' @export
stats::as.stepfun


#' @export
#' 
as.stepfun.picor <- 
function(x, 
         ...)
{
  if (length(x$knots) == 0L) {
    stop("cannot convert 'x' to a stepfun object, as least one knot is needed")
  }
  stats::stepfun(x$knots, x$values, right = TRUE)
}
