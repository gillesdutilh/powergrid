##' @title Scale to desired range
##' @description Linear scaling of a vector to a vector with a required range.
##' @param x A vector
##' @param a The desired minimum
##' @param b The desired maxmum
##' @return a vector with the desired minimum and maximum
##' @author Gilles Dutilh
ScaleRange = function(x, a, b){
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a}
