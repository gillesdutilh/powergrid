## Function developed mainly for internal use in the powergrid packagen. For
## most use cases, you will not need this function, but rather use any of
## \code{\link{Example}}, \code{\link{AddExample}}, \code{\link{PowerPlot}}, or
## \code{\link{GridPlot}}. Only if you need to work with, say, the required n
## for a number of assumptions over and above the standard build in figures,
## there is a need to use FindTarget.
##'
##' FindTarget functions takes as input an array (typically of type
##' `power_array`), e.g., n by effect size, containing at each crossing the
##' power. The function then searches along one dimension (say n) for a value
##' (say, power) of at least (or at most) a chosen target value (say, a power of
##' at least 90%). This is done for each combination of levels of the other
##' dimension(s) (say, effect size by SD).
##'
##' By default, the power_slice is searched along the dimension n
##' (\code{par_to_search}), searching for the lowest value (\code{find_min} = TRUE)
##' where the array contains a value of at least (\code{minimal_target} = TRUE)
##' .9 (the \code{target}), thus finding the minimal sample size required to
##' achieve a power of 90%. These arguments may seem a bit confusing at first,
##' but they allow for three additional purposes:
##'
##' First, the implementation also allows to search for a value that is *at
##' most* the \code{target}, by setting \code{minimal_target} to FALSE. This may
##' be used, for example, when the aim is to find a sample size yielding a
##' confidence interval that is not bigger than some maximum width.
##'
##' Second, the implementation allows to search along another *named* dimension
##' of the power_slice than n.
##'
##' Third, the implementation allows to search for a certain target to be
##' achieved by maximizing (find_minimum = FALSE) the parameter on the searched
##' dimension. This may be used, for example, when the aim is to find the
##' maximum standard deviation at which a study's power is still acceptable.
##'
##' \code{FindTarget} may most often be implicitly called inside
##' \code{\link{Example}}, \code{\link{PowerPlot}} or \code{\link{GridPlot}}.
##'
##' @title Find Target Power or Other Value
##' @param power_slice An array, most commonly of class `power_array`, possibly
##'   the result of taking a slice of an object of class \code{power_array}
##'   using \code{\link{ArraySlicer}} or the power_array []-indexing method.
##' @param target The required value in the power_slice (e.g., .9, if the values
##'   represent power)
##' @param minimal_target Is the target a minimum (e.g., the power) or a maximum
##'   (e.g., the size of a confidence interval)
##' @param par_to_search Which parameter should be searched to achieve the
##'   required target. In the typical power analysis case, this is n.
##' @param find_min If TRUE, the lowest value of par_to_search is found that
##'   yields a value that meets the target. This is typical for n in a sample
##'   size estimation, where one searches the lowest n to achieve a certain
##'   power. For, e.g. the variance, one would however search for the maximum
##'   where the target power can still be achieved.
##' @param method How is the required \code{par_to_search} to achieve
##'   \code{target} found. Either \code{'step'}: walking in steps along
##'   \code{par_to_search} or \code{'lm'}: Interpolating assuming a linear
##'   relation between \code{par_to_search} and \code{(qnorm(x) + qnorm(1 -
##'   0.05)) ^ 2}. Setting 'lm' is inspired on the implementation in the sse
##'   package by Thomas Fabbro.
##' @return Returns an array or vector: containing the value that is found for the
##'   par_to_search (say, n) meeting the target following above criteria (say,
##'   the lowest n for which the power is larger than .9), for each crossing of
##'   the levels of the other dimensions (say, delta, SD).
##' @author Gilles Dutilh
##' @examples
##' ## ============================================
##' ## A basic power analysis example:
##' ## ============================================
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 2),
##'   sig_level = seq(.01, .1, .01),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2), ## effect size
##'   sd = seq(.1, .9, .2)) ## Standard deviation
##' PowFun <- function(n, sig_level, delta, sd){
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = sig_level)
##'   return(ptt$power)
##' }
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##' summary(power_array)
##' 
##' ## We can use Example so find the required sample size, but only for one example:
##' Example(power_array,
##'         example = list(delta = .7, sd = .7, sig_level = .05),
##'         target = .9)
##' 
##' ## If we want to see the required sample size for all delta's, we can use
##' ## FindTarget. Get the minimal n needed for achieving a value of 0.9, at sd =
##' ## .3:
##' n_by_delta_sd_03 = FindTarget(power_array[, sig_level = '0.05', , sd = '0.3'],
##'                               par_to_search = 'n',
##'                               target = .9)
##' 
##' n_by_delta_sd_03
##' ## just as an illustration, a figure (that can be much more aestetically made
##' ## using PowerPlot)
##' plot(as.numeric(names(n_by_delta_sd_03)),
##'      n_by_delta_sd_03, type = 'l')
##' 
##' ## =================================
##' ## Higher dimensionality
##' ## =================================
##' 
##' ## The function works also for higher dimensionality:
##' n_by_delta_sd = FindTarget(power_array,
##'                            par_to_search = 'n',
##'                            target = .85)
##' ## what is the minimum n to achieve .85 for different values of delta, sd, and sig_level:
##' print(n_by_delta_sd)
##' @export
FindTarget = function(power_slice,
                      target = .9,
                      minimal_target = TRUE,
                      par_to_search = 'n',
                      find_min = TRUE,
                      method = 'step'
                      ) {
  ## If simply finding the first step where the target is achieved:
  ## (note very pragmatic use of environmental variables)
  if (method == 'step') {
    Find = function(x) {
      search_par_grid = names(x)
      if(!find_min) {
        x = rev(x)
        search_par_grid = rev(search_par_grid)
      }
      if (minimal_target) {
        first_hit_index = match(TRUE, x >= target)
      } else {
        first_hit_index = match(TRUE, x <= target)
      }
      return(as.numeric(search_par_grid[first_hit_index]))
    }
  }
  if (method == 'lm') {
    ## Implementation of fabbrot's interpolation through
    ## transformation trick.
    if(!minimal_target | !find_min) {
      stop("Currently the lm method only supports defaults for minimal_target and find_min")
    }
    SSETrans <- function(x) {
      ## older versions: 0.5 * log((1 + x) / (1 - x)) # <== this line is
      ## a literal copy of the comment fabbrot in sse
      (stats::qnorm(x) + stats::qnorm(1 - 0.05)) ^ 2
    }
    Find = function(x) {
      pred_n = as.numeric(names(x))
      trans_pow = SSETrans(x)
      lm_out = stats::lm(pred_n[!is.infinite(trans_pow)] ~
                           trans_pow[!is.infinite(trans_pow)])
      lm_pred = stats::predict(lm_out,
                               newdata = data.frame(
                                 trans_pow = SSETrans(target)))
      if(lm_pred %% 1 != 1) warning(paste("The output with method = 'lm' is rounded up, this makes sense for n but may not for parameters"))
      lm_pred = ceiling(lm_pred)
      return(lm_pred)
    }
  }
  ## Below functions rather funnily, maybe, I need to do some explicit argument
  ## naming...
  if(length(dim(power_slice)) %in% c(1, 0) ){ # if a one-dim array, or a vector
    Find(power_slice)
  } else { # is multidim array, you must say which parameter you want to search
                                        # across
    apply(power_slice,
          names(dimnames(power_slice))[
            names(dimnames(power_slice)) != par_to_search],
          Find)
  }
}
