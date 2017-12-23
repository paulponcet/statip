#' @title 
#' Bandwidth calculation
#' 
#' @description 
#' \code{bandwidth} computes the bandwidth to be used in the 
#' \code{\link[statip]{densityfun}} function. 
#' 
#' @param x
#' numeric. The data from which the estimate is to be computed.
#' 
#' @param rule
#' character. A rule to choose the bandwidth. See \code{\link[stats]{bw.nrd}}. 
#' 
#' @return 
#' A numeric value. 
#' 
#' @importFrom stats bw.nrd0 bw.nrd bw.ucv bw.bcv bw.SJ
#' @export
#' 
bandwidth <- 
function(x, rule)
{
  stopifnot(is.character(rule))
  if (length(x) < 2L)
    stop("need at least 2 points to select a bandwidth automatically")
  switch(tolower(rule), 
         nrd0 = stats::bw.nrd0(x), 
         nrd = stats::bw.nrd(x), 
         ucv = stats::bw.ucv(x), 
         bcv = stats::bw.bcv(x), 
         sj = , 
         `sj-ste` = stats::bw.SJ(x, method = "ste"), 
         `sj-dpi` = stats::bw.SJ(x, method = "dpi"), 
         stop("unknown bandwidth rule"))
}
