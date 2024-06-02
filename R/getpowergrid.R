##' @title Get core element from \code{powCal} or \code{powEx} object
##' @description Get from objects resulting from \code{sse::powCal} (class
##'   power) or \code{sse::powEx} (class powCalc) the core element and supply
##'   with appropriate dimnames.
##' @param x Output object from \code{sse::powCal} or \code{sse::powEx}
##' @return array with appropriately named dimensions
##' @author Gilles Dutilh
GetPowergrid = function(x){
  ## x is an object of either class power or powCalc
  ## x = pow
  core = x@core
  dimnames(core) = list(
    n = x@n,
    theta = x@theta,
    xi = x@xi
  )
  return(core)
}
