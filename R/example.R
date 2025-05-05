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
##' 1) In the above description, *minimal* can be changed to *maximal* by setting argument
##' `find_min` to FALSE. This is useful in the situation where one, e.g.,
##' searches for the highest standard deviation at which it is still possible to
##' find a desirable power.
##'
##' 2) In the above description, *at least* can be changed to *at most* by
##' setting `minimal_target` to FALSE. This allows to search, for example, for
##' the minimal sample size where the expected confidence interval is smaller
##' than a certain desired width.
##'
##' @title Find Combination of Parameters Yielding Desired Value (typically
##'   power).
##' @param x Object of class `power_array` or `power`
##' @param example List with named elements representing the constellation of
##'   parameter values for which the example should be found. The names of this
##'   list should match the dimension names of `x`, their values should be exact
##'   values available at these dimensions. See example for an illustration.
##' @param target Which value (of typically power) should be achieved at the
##'   example.
##' @param minimal_target Logical. Should target be minimally achieved (e.g., a
##'   power of at least a certain value), or maximially allowed (e.g., a maximum
##'   expected estimation uncertainty)?
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes a parameter (typically: minimal required n) to achieve the
##'   `target` or maximizes this assumption (e.g., maximal allowed SD).
##' @param method Character string, indicating how the location of the example
##'   is found, passed on internally to `FindTarget`. Either "step": walking in
##'   steps along `search_par` or "lm": Interpolating assuming a linear relation
##'   between `search_par` and (qnorm(x) + qnorm(1 - 0.05)) ^ 2. This method
##'   "lm" is inspired on the implementation in the sse package by Thomas
##'   Fabbro.
##' @param summary_function When x' attribute `summarized` is FALSE, x is
##'   summarized across sims using this function before searching the example.
##' @return Returns a list containing:
##'
##' - "requested_example": the parameters at which the power (or whatever the
##'   values represent) was searched to achieve level `target` (typically
##'   the minimal power, e.g., .9), searching along parameter `required
##'   name` (typically n).
##'
##' - "objective": was `required_name` searched to find the "min" or "max" of
##'   x.
##'
##' - "target": which value should the power (or any other value) have.
##'
##' - "required_name": the parameter searched along to find the minimum (or
##'   maximized if slot `searched` = 'max') to achieve objective. (typically n)
##'
##' - "required_value": the minimum (or maximum if `searched` = "max") for
##'   parameter `required_name` (which is typically n)
##'
##' - "searched": was the "min" or "max" for `required_name` searched?
##'
##' - "minimal_target": Is the target a minimum (TRUE, as typical for power) or a
##'   maximum (FALSE, e.g., an expected uncertainty level)
##' @author Gilles Dutilh
##' @export
##' @examples
##' ## ============================================
##' ## Typical use case: find lowest n for power
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
##'                  target = .9)
##' ex_out #
##'
##' ## ============================================
##' ## Illustration argument `find_min`
##' ## ============================================
##' ##
##' ## In this example, we search for the *highest sd* for which the power is at
##' ## least .9.
##' ex_out = Example(power_array,
##'                  example = list(n = 40, delta = .7),
##'                  target = .9, find_min = FALSE)
##' ex_out # note how the printed result indicates it searched for a maximal
##'                                         # permissible sd.
##'
##' ## ============================================
##' ## Illustration argument `minimal_target`
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
##'                  target = .88,
##'                  find_min = TRUE, # we search the *lowest* n
##'                  minimal_target = FALSE # for a *maximal* mean CI width
##'                  )
##' ex_out # note how the printed result indicates the target CI is a maximum.
##'
##' ## ============================================
##' ## When both `find_min` and `minimal_target` are FALSE
##' ## ============================================
##' ##
##' ## In this example, we search for the *highest sd* for which the average CI
##' ## width is still *smaller than or equal to .88*.
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, n = 60),
##'                  target = .88,
##'                  find_min = FALSE, # we search the *highest* sd
##'                  minimal_target = FALSE # for a *maximal* mean CI width
##'                  )
##'
##' ex_out # note how the printed result indicates that the *maximal permissible SD*
##'        # was found for a CI of *at most .88*.
##'
Example = function(x,
                   example = NULL,
                   target = NULL,
                   minimal_target = TRUE,
                   find_min = TRUE,
                   method = 'step',
                   summary_function = mean){
  ## =======================================================
  ## Warnings and Errors depending on input
  ## warnings for atypical input
  if(inherits(x, 'power')) {
    warning(PrintWrap("You supplied an object x of type 'power', likely from the sse package. This will only work correctly when the sse package is loaded."))
  } else {
    if (all(class(x) == 'power_array') && !is.na(attr(x, which = 'n_iter')) &&
        !attr(x, which = 'summarized')){
      x = SummarizeSims(x, summary_function)
      warning(PrintWrap("The object 'x' you supplied to Example() contains individual iterations. For finding an example, these were automatically summarized across simulations using the function given in argument `summary_function`."), call. = FALSE)
      }
  }
  if (!inherits(x, 'power') & is.null(target)){
    stop("An example can only be found when `target` is given (currently, target = NULL).")
  }
  if (!inherits(x, 'power') & (length(dimnames(x)) - length(example)) != 1){
    stop("The argument `example` should slice out a one-dimensional vector from `x` to find the example on.")
    }
  ## general warning lm
  if (method == 'lm' && any(target %in% 0:1)){
    stop(PrintWrap("Method is set to 'lm', which only makes sense for power as a function of n. Searching for a power of 1 or 0 is not supported by this package. For help achieving a power of 1 or 0, see a priest or a shrink, respectively."))}
  if (all(class(x) != 'power_array' &
          class(x) != 'pseudo_power_array_by_plotpower' &
          !inherits(x, 'power'))){
    ## just throw an error (may be implemented later)
    stop("The object 'x' should be of class 'power_array' or 'powCalc' (from package 'sse'). ")
  }

  ## =======================================================
  ## If the input is not rejected, adjust atypical input.
  ##
  ## This function may get input with class `power` from sse package (typically
  ## when applied inside the plot function). Confusingly, such object is the
  ## result from powEx and thus already contains the example. 'power' input is
  ## turned into class 'power_example'.
  if(inherits(x, 'power')) {
    example_list = list(
      requested_example = list(theta = sse::tex(x, type = 'theta')),
      objective = 'achieve target or higher',
      target = sse::tex(x, type = 'power'),
      required_name = 'n',
      required_value = sse::tex(x, type = 'nRec'),
      searched = 'min',
      method = attr(x, which = "method"),
      objective = 'achieve target or higher'
    )
  } else if (all(class(x) %in% c('power_array',
                                 'pseudo_power_array_by_plotpower'))){
    ## =========================================================================
    ## Standard situation: -----------------------------------------------------
    ## when it is a regular `power_array` object, find the min/max for target
    ## =========================================================================
    slice_to_search = ArraySlicer(x, example)
    required_value = FindTarget(slice_to_search,
                                target = target,
                                minimal_target = minimal_target,
                                search_par = 'this is ignored for vector',
                                find_min = find_min,
                                method = method)
    required_name = attr(slice_to_search, which = 'dims_left')
    ## required_name = names(dimnames(slice_to_search))
    ## x_ex_name = names(margins_toplot)[names(margins_toplot) != par_to_search]
    ## x_ex_value = example[[x_ex_name]]
    ## note that "y_ex_name" is not defined, this is par_to_search
    example_list = list(
      requested_example = example,
      objective = ifelse(minimal_target,
                         'achieve target or higher',
                         'achieve target or lower'),
      target = target,
      required_name = required_name,
      required_value = required_value,
      searched = ifelse(find_min, 'min', 'max'),
      method = method)
  }
  if (method == 'step' & is.na(example_list$required_value)){
    warning(
      paste0('No value of ', required_name, ' evaluated where target of ',
             target,
             ' was achieved.\nConsider using method = "lm" for extrapolation.'))
  }
  class(example_list) = 'power_example'
  return(example_list)
}

##' Print method for class `power_example`.
##'
##' Print short informative output for object of class `power_example`.
##'
##' @title Print Example
##' @param x object of class `power_example`
##' @return nothing
##' @author Gilles Dutilh
##' @export
power_example = function(x, ...){
  description =
    ifelse(x$method == 'lm',
           paste0('\nDescription: Method "lm" was chosen to use interpolation to approach the ',
                  ifelse(x$searched == 'min', 'lowest ', 'highest '),
                  x$required_name, ' that yields a target (e.g., power) of ',
                  ifelse(x$objective == 'achieve target or higher',
                         'at least ', 'at most '), x$target,
                  '.\nThis interpolation only makes sense for studying the relation between n and power.'),
           paste0('\nDescription: Method "step" was used to find the ',
                  ifelse(x$searched == 'min', 'lowest ', 'highest '),
                  x$required_name,
                  ' in the searched grid that yields a target (typically power) of',
                  ifelse(x$objective == 'achieve target or higher',
                          ' at least ', ' at most '), x$target, '.')
           )
  content = paste0('To achieve the target of ',
             ifelse(x$objective == 'achieve target or higher',
                    'at least ', 'at most '),
             x$target,
             ' assuming\n',
             paste(names(x$requested_example),
                   x$requested_example, sep = ' = ', collapse = '\n'),
             ',\n',
             ifelse(x$searched == 'min', 'the minimal required ', 'maximal permissible '),
             x$required_name, ' = ', x$required_value,
             '\n------------------------------------------------\n',
             paste0(strwrap(description, 48), collapse = '\n')
             )
  cat('================================================\n')
  cat(content, ...)
  cat('\n================================================\n')
}

##' Summary method for class `power_example`.
##'
##' Print longer informative output for object of class `power_example`.
##'
##' @title Print Example
##' @param x object of class `power_example`
##' @return nothing
##' @author Gilles Dutilh
##' @export
summary.power_example = function(x, ...){
  data.frame('example' = unlist(x), ...)
}

