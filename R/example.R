##' Find combination of parameters yielding desired power (or any other target
##' value) in an object of class "power_array".
##'
##' In the most typical use case, and this is also the default, `Example`
##' searches the *minimal* n where the power is *at least* equal to the value
##' given by argument `target`. The function is, however, designed much more
##' generically. The explanation below may be less helpful than trying the
##' examples, but for completeness:
##'
##' Argument `example` slices out a vector from object `x`, representing the
##' values at the parameter combination given in example, thus, along the
##' remaining parameter. Then, `Example` searches along this vector for the
##' *minimal* parameter value where the value of the vector is *at least* equal
##' to `target`. Thus, if the sliced out vector contains values of "power" along
##' the parameter "effect size", it searches the minimal effect size at which
##' the target power is achieved.
##'
##' Two complications are made to allow for complete flexibility:
##'
##' 1) In the above description, *minimal* can be changed to *maximal* by
##' setting argument `find_lowest` to FALSE. This is useful in the situation where
##' one, e.g., searches for the highest standard deviation at which it is still
##' possible to find a desirable power.
##'
##' 2) In the above description, *at least* can be changed to *at most* by
##' setting `target_at_least` to FALSE. This allows to search, for example, for
##' the minimal sample size where the expected confidence interval is smaller
##' than a certain desired width.
##'
##' Example searches for the minimum or maximum on one parameters (say, the
##' minimum n) given *one single constellation* of further parameters. However,
##' you may want to study how, say, the required n (or any other value) depends
##' on the value of further parameters. The functions PowerPlot and GridPlot
##' offer plotting functionalities to graphically illustrate such
##' dependencies. If you want to find "Examples" as a function of parameter
##' settings and work with these, you can use the workhorse behind 'Example',
##' PowerPlot and Gridplot, \code{\link{FindTarget}}
##' @title Find combination of parameters required for achieving a desired power
##'   (or other objective).
##' @param x Object of class `power_array`
##' @param example List with named elements representing the constellation of
##'   parameter values for which the example should be found. The names of this
##'   list should match the dimension names of `x`, their values should be exact
##'   values available at these dimensions. See example for an illustration.
##' @param target_value Which value (of typically power) should be achieved at
##'   the example.
##' @param target_at_least Logical. Set to TRUE if you aim to achieve a minimum
##'   value (e.g., a power must be *at least* 90%), or FALSE if you want to
##'   allow a maximum value (e.g., the width of the expected CI may be *at most*
##'   a certain value).
##' @param find_lowest Logical, indicating whether the example should be found
##'   that minimizes a parameter (typically: minimal required n) to achieve the
##'   `target_value` or maximizes this assumption (e.g., maximal allowed SD).
##' @param method Character string, indicating how the location of the example
##'   is found, passed on internally to `FindTarget`. Either "step": walking in
##'   steps along the parameter of interest or "lm": Interpolating assuming a
##'   linear relation between the parameter of interest and (qnorm(x) + qnorm(1
##'   - 0.05)) ^ 2. This method "lm" is inspired on the implementation in the
##'   sse package by Thomas Fabbro.
##' @param summary_function When x' attribute `summarized` is FALSE, x is
##'   summarized across iterations using this function before searching the
##'   example.
##' @seealso \code{\link{PowerGrid}}, \code{\link{FindTarget}},
##'   \code{\link{PowerPlot}}, \code{\link{GridPlot}}
##' @return Example returns a list containing:
##'
##' - "requested_example": the parameter combination at which the power (or
##'   whatever the values represent) was searched to achieve level `target_value`
##'   (typically the minimal power, e.g., .9), searching along parameter `required
##'   name` (typically n).
##'
##' - "objective": was `required_name` searched to find the "min" or "max" of
##'   x?
##'
##' - "target_value": which value should the power (or any other value) have?
##'
##' - "required_name": the parameter searched along to find the minimum (or
##'   maximized if slot `searched` = 'max') to achieve objective. (typically n)
##'
##' - "required_value": the minimum (or maximum if `searched` = "max") for
##'   parameter `required_name` (which is typically n)
##'
##' - "searched": was the "min" or "max" for `required_name` searched?
##'
##' - "target_at_least": Is the target_value a minimum (TRUE, as typical for power) or a
##'   maximum (FALSE, e.g., an expected uncertainty level)?
##' @author Gilles Dutilh
##' @examples
##' ## ============================================
##' ## Typical use case: find lowest n for a certain target power
##' ## ============================================
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 2),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.1), ## effect size
##'   sd = seq(.1, .9, .2)) ## Standard deviation
##' PowFun <- function(n, delta, sd){
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = 0.05)
##'   return(ptt$power)
##' }
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##' ##'
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, sd = .7),
##'                  target_value = .9)
##' ex_out #
##'
##' ## ============================================
##' ## Illustration argument `find_lowest`
##' ## ============================================
##' ##
##' ## In this example, we search for the *highest sd* for which the power is at
##' ## least .9.
##' ex_out = Example(power_array,
##'                  example = list(n = 40, delta = .7),
##'                  target_value = .9, find_lowest = FALSE)
##' ex_out # note how the printed result indicates it searched for a maximal
##'                                         # permissible sd.
##'
##' ## ============================================
##' ## Illustration argument `target_at_least`
##' ## ============================================
##' ##
##' ## In the example below, we search for the lowest n where the expected CI-width
##' ## is not larger than .88.
##' PowFun <- function(n, delta, sd){
##'   x1 = rnorm(n = n/2, sd = sd)
##'   x2 = rnorm(n = n/2, mean = delta, sd = sd)
##'   CI_width = diff(t.test(x1, x2)$conf.int) # CI95 is saved
##' }
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 5),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2),
##'   sd = seq(.5, 1.5, .2))
##' ## we iterate, and take the average across iterations to get expected CI-width:
##' n_iter = 20
##' set.seed(1)
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = n_iter)
##' summary(power_array)
##' ## Now, find lowest n for which the average CI width is *smaller than .88*.
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, sd = .7),
##'                  target_value = .88,
##'                  find_lowest = TRUE, # we search the *lowest* n
##'                  target_at_least = FALSE # for a *maximal* mean CI width
##'                  )
##' ex_out # note how the printed result indicates the target CI is a maximum.
##'
##' ## ============================================
##' ## When both `find_lowest` and `target_at_least` are FALSE
##' ## ============================================
##' ##
##' ## In this example, we search for the *highest sd* for which the average CI
##' ## width is still *smaller than or equal to .88*.
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, n = 60),
##'                  target_value = .88,
##'                  find_lowest = FALSE, # we search the *highest* sd
##'                  target_at_least = FALSE # for a *maximal* mean CI width
##'                  )
##'
##' ex_out # note how the printed result indicates that the *maximal permissible SD*
##'        # was found for a CI of *at most .88*.
##'
##' @export
Example = function(x,
                   example = NULL,
                   target_value = NULL,
                   target_at_least = TRUE,
                   find_lowest = TRUE,
                   method = 'step',
                   summary_function = mean){
  ## =======================================================
  ## Warnings and Errors depending on input
  ## warnings for atypical input
  if (all(class(x) == 'power_array') && !is.na(attr(x, which = 'n_iter')) &&
      !attr(x, which = 'summarized')){
    x = SummarizeIterations(x, summary_function)
    warning("The object 'x' you supplied to Example() contains individual iterations. For finding an example, these were automatically summarized across iterations using the function given in argument `summary_function`.", call. = FALSE)
  }
  if (is.null(target_value)){
    stop("An example can only be found when `target_value` is given (currently, target_value = NULL).")
  }
  if ((length(dimnames(x)) - length(example)) != 1){
    stop("The argument `example` should slice out a one-dimensional vector from `x` to find the example on.")
  }
  ## general warning lm
  if (method == 'lm' && any(target_value %in% 0:1)){
    stop("Method is set to 'lm', which only makes sense for power as a function of n. Searching for a power of 1 or 0 is not supported by this package. For help achieving a power of 1 or 0, see a priest or a shrink, respectively.")}
  if (all(class(x) != 'power_array')){
    ## just throw an error (may be implemented later)
    stop("The object 'x' should be of class 'power_array'.")
  }
  
  ## =======================================================
  ## If the input is not rejected, adjust atypical input.
  ##
  if (all(class(x) =='power_array')){
    ## =========================================================================
    ## Standard situation: -----------------------------------------------------
    ## when it is a regular `power_array` object, find the min/max for target
    ## =========================================================================
    slice_to_search = ArraySlicer(x, example)
    required_value = FindTarget(slice_to_search,
                                target_value = target_value,
                                target_at_least = target_at_least,
                                par_to_search = 'this is ignored for vector',
                                find_lowest = find_lowest,
                                method = method)
    required_name = attr(slice_to_search, which = 'dims_left')
    example_list = list(
      requested_example = example,
      objective = ifelse(target_at_least,
                         'achieve target_value or higher',
                         'achieve target_value or lower'),
      target_value = target_value,
      required_name = required_name,
      required_value = required_value,
      searched = ifelse(find_lowest, 'min', 'max'),
      method = method)
  }
  if (method == 'step' & is.na(example_list$required_value)){
    warning(
      paste0('No value of ', required_name, ' evaluated where target of ',
             target_value,
             ' was achieved.\nConsider using method = "lm" for extrapolation.'))
  }
  class(example_list) = 'power_example'
  return(example_list)
}

##' Print method for class `power_example`.
##'
##' Print short informative output for object of class `power_example`.
##'
##' @title Print example
##' @param x object of class `power_example`
##' @param ... passed on to `cat`
##' @return nothing
##' @author Gilles Dutilh
##' @export
print.power_example = function(x, ...){
  description =
    ifelse(x$method == 'lm',
           paste0('\nDescription: Method "lm" was chosen to use interpolation to approach the ',
                  ifelse(x$searched == 'min', 'lowest ', 'highest '),
                  x$required_name, ' that yields a target value (e.g., power) of ',
                  ifelse(x$objective == 'achieve target_value or higher',
                         'at least ', 'at most '), x$target_value,
                  '.\nThis interpolation only makes sense for studying the relation between n and power.'),
           paste0('\nDescription: Method "step" was used to find the ',
                  ifelse(x$searched == 'min', 'lowest ', 'highest '),
                  x$required_name,
                  ' in the searched grid that yields a target_value (typically power) of',
                  ifelse(x$objective == 'achieve target_value or higher',
                         ' at least ', ' at most '), x$target_value, '.')
           )
  example_in_words =
    paste0('To achieve the target value of ',
           ifelse(x$objective == 'achieve target or higher',
                  'at least ', 'at most '),
           x$target_value,
           ifelse(all(is.null(x$requested_example)),
                  ", ", # if only one dimension left
                  paste0(' assuming\n',
                         paste(names(x$requested_example),
                               x$requested_example, sep = ' = ', collapse = '\n'),
                         ',\n')),
           ifelse(x$searched == 'min', 'the minimal required ', 'maximal permissible '),
           x$required_name, ' = ', x$required_value)
  content = paste0(
    ifelse(all(is.null(x$requested_example)),
           PrintWrap(example_in_words), example_in_words),
    '\n',
    PrintDashes('-'),
    '\n',
    PrintWrap(description)
  )
  cat_print = paste0(paste(c(
    PrintDashes(),
    content,
    PrintDashes()), collapse = '\n'), '\n') 
  cat(cat_print, ...)
}

##' Summary method for class `power_example`.
##'
##' Print longer informative output for object of class `power_example`.
##'
##' @title Print contents of an example
##' @param object object of class `power_example`
##' @param ... passed on to `data.frame` (which is the thing that is printed)
##' @return nothing
##' @author Gilles Dutilh
##' @export
summary.power_example = function(object, ...){
  data.frame('example' = unlist(object), ...)
}

