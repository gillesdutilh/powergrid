## Aim:
##
## - take a list of parameters apply function to grid of parameters form usable
## - output in form of an array (may be converted to grid)

##' FillGrid Is an apply-like function, allowing to evaluate a function at the
##' crossings of a set of parameters. The results is saved in an array with
##' attributes that optimize further usage by functions in package
##' \code{powergrid}. In particular, performing a function iteratively (using
##' parallel computing if required) is implemented conveniently. The typical use
##' is for evaluating statistical power at a grid of assumed parameters.
##'
##' Function \code{fun} is evaluated at each combination of the argument values
##' listed in \code{pars} and its results are stored in an array whose
##' dimensions (and \code{dimnames()}) are defined by \code{pars}. If function
##' \code{fun} returns more than one value (always as a single vector), the
##' array will have an additional dimension, with levels named after the names
##' of the returned vector (if given). If n_iter is not NA, function \code{fun}
##' is evaluated n_iter times. This will add to the resulting array an
##' additional dimension "sim".
##'
##' The elements of \code{pars} must match the argument names of fun. If input
##' parameters are not to be part of the grid, but rather further settings,
##' these can be passed on in a named list to \code{fun} through the argument
##' \code{more_args}.
##'
##' @title Evaluate Function (iteratively) at Grid of Input Arguments
##' @param pars A list where each element is a vector of values named as one of
##'   the arguments of \code{fun}. Fun will be applied to the full grid crossing
##'   the values of each of these parameters.
##' @param fun Function applied at each combination of \code{pars}. Arguments
##'   may contain all element names of \code{pars} and \code{more_args}. Output
##'   should be a single number or a vector.
##' @param more_args Fixed arguments to \code{fun} that are not in
##'   \code{pars}. (internally used in \code{.mapply} for supplying argument
##'   \code{MoreArgs})
##' @param n_iter If not NA, function \code{fun} is applied \code{n_iter} times
##'   at each point in the grid defined by \code{pars}.
##' @param summarize Logical indicating whether iterations (if \code{n_iter} is
##'   given) are to be sunmmarized by \code{summary_function}.
##' @param summary_function A function to be applied to aggregate across
##'   simulations. Defaults to \code{mean}, ignored when \code{keep_sims} ==
##'   TRUE or when \code{is.na(n_iter)}.
##' @param parallel Should parallel computing be applied. If TRUE,
##'   future::future_replicate is used.
##' @param n_cores Passed on to future_replicate
##' @return An array of class "power_array"
##' @author Gilles Dutilh
##'
##' @examples
##' ## ============================================
##' ## most basic case, power function available:
##' ## ============================================
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 2),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2), ## effect size
##'   sd = seq(.1, .9, .2)) ## Standard deviation
##' PowFun <- function(n, delta, sd){
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = 0.05)
##'   return(ptt$power)
##' }
##' power_array = FillGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##' ##'
##' summary(power_array)
##'
##' ## ============================================
##' ## Multiple outputs are automatically handled
##' ## ============================================
##' TwoValuesFun <- function(n, delta, sd){
##'   p5 = power.t.test(n = n, delta = delta, sd = sd, sig.level = .05)$power
##'   p1 = power.t.test(n = n, delta = delta, sd = sd, sig.level = .01)$power
##'   return(c('p5' = p5, 'p1' = p1))
##' }
##' ##
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 2),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2),
##'   sd = seq(.5, 1.5, .2))
##' array_two_returns = FillGrid(sse_pars, TwoValuesFun)
##' ## multiple outputs result in an additional dimension:
##' dimnames(array_two_returns)
##' summary(array_two_returns)
##'
##' ## ============================================
##' ## Simulations over iterations
##' ## ============================================
##' PowFun <- function(n, delta, sd){
##'   x1 = rnorm(n = n/2, sd = sd)
##'   x2 = rnorm(n = n/2, mean = delta, sd = sd)
##'   t.test(x1, x2)$p.value < .05
##' }
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 5),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2),
##'   sd = seq(.5, 1.5, .2))
##' ##
##' n_iter = 20
##' power_array = FillGrid(pars = sse_pars, fun = PowFun,
##'                        n_iter = n_iter)
##' dimnames(power_array)

FillGrid = function(pars, fun, more_args = NULL, n_iter = NA,
                    summarize = TRUE, summary_function = mean,
                    parallel = FALSE,
                    n_cores = future::availableCores()-1) {
  ##
  ## ============================================
  ## Process arguments
  ##
  ## catch the very unlikely case someone takes e1d42fl5z7b6 as a
  ## parameter name, or a parameter name including funout_
  if ('e1d42fl5z7b6' %in% names(pars) || any(grepl('funout_', names(pars)))) {
    stop('You chose one of few parameter names that are not allowed (e1d42fl5z7b6 or funout_...)')}
  ## All pars arguments of fun?
  if (!all(names(pars) %in% methods::formalArgs(fun))){
    stop("`pars` contains parameters that do not match the arguments of `fun`")
  }
  ## ============================================
  ## fill grid
  pars_grid = expand.grid(pars)
  ## =================================
  ## No simulation ('n_iter' not supplied)
  if(is.na(n_iter)) {
    e1d42fl5z7b6 = sapply( # the long name is to make it very unlikely
                                        # to get the same name in the grid, which
                                        # would break the xtab below.
      .mapply(fun, pars_grid, MoreArgs = more_args), function(x)x,
      simplify = "array")
    ## out = cbind(pars_grid, e1d42fl5z7b6)
    ## result is a n_result_vars by nrow(pars_grid) matrix
  }
  ##'
  ## =================================
  ## Simulation ('n_iter' supplied)
  if (!is.na(n_iter) && !parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Setting argument `parallel' to TRUE requires installation of future.apply", call. = FALSE)}
    e1d42fl5z7b6 =
      drop(replicate(
        n_iter, sapply( # reshape mapply result
                  .mapply(fun, pars_grid, MoreArgs = more_args),
                  function(x)unlist(x))))
    if (summarize) {
      e1d42fl5z7b6 = apply(e1d42fl5z7b6,
                           1:(length(dim(e1d42fl5z7b6)) - 1), # per all but sim dim
                           summary_function, simplify = TRUE)
    }
  }
  ##'
  ## parallel using future_replicate
  if (!is.na(n_iter) && parallel) {
    ## plan(strategy = future_args$plan$strategy, # 'multisession'
    ##      workers = future_args$plan$workers) # future::availableCores() - 1)
    future::plan("future::multisession", workers = n_cores)
    e1d42fl5z7b6 =
      drop(future.apply::future_replicate(
                           n_iter, sapply( # reshape mapply result
                                     .mapply(fun, pars_grid, MoreArgs = more_args),
                                     function(x)unlist(x))
                         ))
    if (summarize){
      e1d42fl5z7b6 = apply(e1d42fl5z7b6,
                           1:(length(dim(e1d42fl5z7b6)) - 1), # per all but sim dim
                           summary_function, simplify = TRUE)
    }
  }
  ## =================================
  n_funouts = length(.mapply(fun, pars_grid[1, ], MoreArgs = more_args)[[1]])
  ##'
  ## Turn grid into array
  ToArray = function(gg){
    stats::xtabs(stats::as.formula(
                          paste('gg ~', paste(names(pars_grid), collapse = '+'))), data = pars_grid)
  }
  ## Since the array resulting from .mapply may differ depending on
  ## 1) all.equal(summary_function, I) vs other summary function, vs no iterations
  ## 2) having one versus more than one result variable,
  ## I transform into an output array in the IFs below.
  ##'
  ## Simplest situation, with no iterations or summarized iterations,
  ## only one variable.
  if (n_funouts == 1 && (summarize || is.na(n_iter))) {
    out_array = ToArray(e1d42fl5z7b6)
  } # simple xtabs with pars
  ##'
  ## With no iterations or summarized iterations,
  ## but multiple variables
  if (n_funouts > 1 && (summarize || is.na(n_iter))) {
    ## first take care that pars names and funout names are not confused
    if(any(rownames(e1d42fl5z7b6) %in% names(pars))){
      rownames(e1d42fl5z7b6) =
        paste0('funout_', rownames(e1d42fl5z7b6))}
    ## transform to array
    out_array = ToArray(t(e1d42fl5z7b6))
    names(dimnames(out_array))[length(dim(out_array))] = 'fun_out'
  } # simple xtabs on transposed array
  ##'
  ## When simulations are not summarized (kept), one variable
  if (n_funouts == 1 && !summarize && !is.na(n_iter)) {
    out_array = ToArray(e1d42fl5z7b6) # so maybe merge with above
    dimnames(out_array)[length(dimnames(out_array))] = NULL
    names(dimnames(out_array))[length(dimnames(out_array))] = 'sim'
  }
  ##'
  ## When simulations are not summarized (kept), multiple variables
  if (n_funouts > 1 && !summarize && !is.na(n_iter)) {
    ## first take care that pars names and funout names are not confused
    if(any(dimnames(e1d42fl5z7b6)[[1]] %in% names(pars))){
      dimnames(e1d42fl5z7b6)[[1]] =
        paste0('funout_', dimnames(e1d42fl5z7b6)[[1]])}
    ## put in flat format to work with xtabs later
    flat = cbind(
      as.data.frame(stats::ftable(e1d42fl5z7b6, row.vars = c(2, 1, 3))),
      apply(pars_grid, 2, rep, n_funouts * n_iter))
    ## easiest to set dimnames before xtabs
    colnames(flat)[1:4] = c('parscom', 'fun_out', 'sim', 'value')
    out_array = stats::xtabs(value ~ ., data = flat[, -1])
    dimnames(out_array)[['sim']] = seq_along(dimnames(out_array)[['sim']])
    ## Sort such that the first dimensions are the pars
    dimnums = seq_along(dim(out_array))
    pardimnums = dimnums[!(dimnums %in% 1:2)]
    out_array = aperm(out_array, c(pardimnums, 1:2))
    ##'
    ##'
  }
  ##'
  ## set attributes of output object
  class(out_array) = 'power_array'
  attr(out_array, which = 'sim_function') = fun
  attr(out_array, which = 'sim_function_nval') = n_funouts
  attr(out_array, which = 'pars') = pars
  attr(out_array, which = 'more_args') = more_args
  attr(out_array, which = 'summarized') = summarize
  attr(out_array, which = 'n_iter') = n_iter
  attr(out_array, which = 'summary_function') =
    substitute(summary_function)
  attr(out_array,
       which = 'n_iter') = ifelse(is.na(n_iter), NA, n_iter)
  return(out_array)
}

## ==================================================================
## Method for [INDEXING]
## ==================================================================
## in order to keep class and other attributes

##' Method for indexing [] of objects of class power_array. The method
##' makes sure that the resulting array is of class power_array and
##' keeps and updates the object's attributes. These attributes are
##' needed for various functions in the powergrid package to work
##' well.
##' ##'
##' The indexing functions as normal indexing, but note that drop is
##' FALSE by default, so that the resulting array has the same
##' dimensions as the original array. The number of levels at each
##' dimension may be reduced, however.
##' ##'
##' @title indexing with [ ] for class power_array
##' @return An array of class power_grid
##' @author Gilles Dutilh
##' @param x object
##' @param ... index
##' @param drop drop
`[.power_array` <- function(x, ..., drop=TRUE) {
  the_attributes = attributes(x)
  x = NextMethod(x)
  the_attributes$dim = dim(x)
  the_attributes$dimnames = dimnames(x)
  if (all(names(dimnames(x)) != 'fun_out')){
    the_attributes$sim_function_nval = 1
  } else {the_attributes$sim_function_nval = length(dimnames(x)$fun_out)}
  attributes(x) = the_attributes
  return(x)
}

## ==================================================================
## Method for printing power_array
## ==================================================================

##' Method for printing objects of class power_array.
##' ##'
##' Prints a power_array as a default array with a short summary about
##' its contents.
##' @title print
##' @param x object of class power_array
##' @return object of class power_array
##' @author Gilles Dutilh
print.power_array = function(x){
  print.table(x)
  cat(
    paste0('Array of class "power_array" created using FillGrid, ',
           ifelse(!is.na(attr(x, which = 'n_iter')) &
                  !attr(x, which = 'summarized'),
                  '\nkeeping individual simulations,\n',
                  ''),
           ifelse(attr(x, which = 'sim_function_nval') > 1,
                  paste0(attr(x, which = 'sim_function_nval'), ' output values from each application of fun,\n'), ''),
           'yielding dimensions:\n',
           paste(names(dimnames(x)), collapse = ', '), '. ',
           ifelse(!is.na(attr(x, which = 'n_iter')),
                  paste0('\nResults from ', attr(x, which = 'n_iter'), ' iterations\n'),
                  paste0('')
                  ),
           ifelse(!is.na(attr(x, which = 'n_iter')) & attr(x, which = 'summarized'),
                  "summarized by `summary_function` (see attributes).", ""),
           '\n'
           )
  )
}

## ==================================================================
## Method for summary power_array
## ==================================================================

##' Offers a short summary of the power_array object, summarizing the
##' range of observed values and the grid evaluated across.
##' ##'
##' See PowerGrid for details
##' @title Summary of power_grid object.
##' @param x array of class power_grid
##' @author Gilles Dutilh
summary.power_array = function(x){
  aa = attributes(x)
  parnames = names(aa$dimnames)
  if(!is.na(aa$n_iter) && aa$summarized){
    iter_summary_text = paste0(paste(strwrap(paste0('- Containing summary statistic over ',
                                                    aa$n_iter, ' iterations. See attribute summary_function for the applied summary statistic.'), width = 50), collapse = '\n  '), '\n')
  }
  if(!is.na(aa$n_iter) && !aa$summarized){
    iter_summary_text = paste0('- Containing output of ', aa$n_iter, ' individual iterations.\n')
    parnames = parnames[-length(parnames)] # get rid of the sim dimension
  }
  if(is.na(aa$n_iter)){
    iter_summary_text = ''
  }
  cat(paste0(
    "- Object of class: ", aa$class, "\n",
    iter_summary_text,
    "- Range of values: ",
    ifelse('fun_out' %in% names(dimnames(x)),
           paste0('\n    ', dimnames(x)$fun_out, ': ',
                  apply(apply(x, 'fun_out', range, na.rm = TRUE),
                        2, function(x)
                        {paste0(
                           '[', paste0(round(x, 2), collapse = ', '), ']')}), collapse = '')
         , paste0('[', paste0(round(range(x, na.rm = TRUE), 2), collapse = ', '), ']')
           )
  , "\n- Evaluated at:\n",
    ## paste(format(parnames, width = max(nchar(parnames) + 2), justify = 'right'),
    ##       lapply(aa$dimnames, paste, collapse = ', '),
    ##       sep = ': ', collapse = '\n'),
    paste(mapply(function(x, y){
      paste(format(as.list(x), width = max(nchar(parnames) + 2), justify = 'right'),
            strwrap(paste(y, collapse = ', '), width = 50), collapse = '\n')},
      x = as.list(parnames), y = aa$dimnames[parnames]), collapse = '\n'),
    '\n')
    )
}

## summary(out_single)
## attributes(out_single)


