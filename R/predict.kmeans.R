
#' @importFrom stats na.pass
#' @export
#' 
stats::na.pass


#' @importFrom clue cl_predict
#' @export
#' 
predict.kmeans <- 
function(object, 
         newdata, 
         #na.action = na.pass,
         ...) 
{
  if (missing(newdata) || is.null(newdata)) return(object$cluster)
  x <- clue::cl_predict(object, newdata, type = "class_ids")
  as.integer(x)
}
