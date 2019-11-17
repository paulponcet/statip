#' @title 
#' Default model predictions
#' 
#' @description 
#' Default method of the \code{\link[stats]{predict}} generic 
#' function, which can be used when the model object is empty. 
#' 
#' @param object
#' A model object, possibly empty. 
#' 
#' @param newdata
#' An optional data frame in which to look for variables 
#' with which to predict. 
#' If omitted, the fitted values are used.
#' 
#' @param ...
#' Additional arguments.
#' 
#' @return 
#' A vector of predictions. 
#' 
#' @seealso 
#' \code{\link[stats]{predict}} from package \pkg{stats}.
#' 
#' @export
#' 
#' @examples 
#' stats::predict(NULL)
#' stats::predict(NULL, newdata = data.frame(x = 1:2, y = 2:3))
#' 
predict.default <-
function(object,
         newdata,
         ...)
{
  if (length(object) == 0L) {
    n <- if (missing(newdata)) 0L else nrow(newdata)
    return(rep(NA, n))
  }
  NextMethod("predict")
}


#' @importFrom stats predict
#' @export
#' 
stats::predict
