##' Find combination of parameters yielding desired power (or other value) in an
##' object of class `power_array`.
##'
##' In the most common use case, and this is also the default, `Example`
##' searches the *minimal* n where the power is *at least* equal to the value
##' given by argument `target`. More generically, the object x, sliced at
##' the values given in argument `example` results in a vector. This vector
##' is searched for the *minimal* value of the names of the vector where the
##' value of the vector is *at least* equal to `target`. Thus, when the
##' example is a vector of power at different effect sizes, it searches the
##' minimal effect size at which the target power is achieved.
##'
##' Above, *minimal* is defined by the default value TRUE of argument
##' `find_min`. If `find_min` is FALSE, the maximum is searched. This
##' is useful in the situation where one searches for the highest standard
##' deviation at which it is still possible to find a desirable power.
##'
##' Above, *at least* is defined by the default value TRUE
##' argument `minimal_target`. Setting `minimal_target` to FALSE allows
##' to search, for example, for the minimal sample size where the expected
##' confidence interval is smaller than a certain desired width.
##'
##' @title Find Combination of Parameters Yielding Desired Value (typically
##'   power).
##' @param x Object of class `power_array` or `power`
##' @param example List with named elements representing the parameter values at
##'   which the example should be based. The names of this list should match the
##'   dimension names of `x`, their values should be exact values at these
##'   dimensions. Note that these are not indices, but values. See example for
##'   an illustration.
##' @param target Which value (of typically power) should be achieved at the
##'   example.
##' @param minimal_target Logical. Should target be minimally achieved (e.g.,
##'   power), or maximially allowed (e.g., estimation uncertainty)?
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes an assumption (e.g., minimal required n) to achieve the `target`
##'   or maximizes this assumption (e.g., maximally allows SD).
##' @param method Character string, indicating how the location of the example
##'   is found, as implemented in `FindTarget`. Either `"step"`: walking in
##'   steps along `search_par` or `"lm"`: Interpolating assuming a linear
##'   relation between `search_par` and (qnorm(x) + qnorm(1 - 0.05)) ^
##'   2. Setting `"lm"` is inspired on the implementation in the sse package by
##'   Thomas Fabbro.
##' @param summary_function When x' attribute `summarized` is FALSE, x is
##'   summarized across sims using this function.
##' @return Returns a list containing:
##'
##' - requested_example: the parameters at which the power (or whatever the
##'   values represent) was searched to achieve level `target` (typically
##'   the minimal power, e.g., .9), searching along parameter `required
##'   name` (typically n).
##'
##' - objective: was `required_name` searched to find the "min" or "max" of
##'   x.
##'
##' - target: which value should the power (or any other value) have.
##'
##' - required_name: the parameter searched along to find the minimum (or
##'   maximized if slot `searched` = 'max') to achieve objective. (typically n)
##'
##' - required_value: the minimum (or maximum if `searched` = "max") for
##'   parameter `required_name` (which is typically n)
##'
##' - searched: was the "min" or "max" for `required_name` searched?
##'
##' - minimal_target: Is the target a minimum (TRUE, as typical for power) or a
##'   maximum (FALSE, e.g., an expected uncertainty level)
##' @author Gilles Dutilh
##' @examples
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
##'
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, sd = .7),
##'                  target = .9)
##' ex_out
##'
##' ## Concerning argument `example`: The values in `example` refer to values of
##' #the # parameters, and are *not indices*. This means that below, `delta = 1`
##' #refers to the # *value* 1.0, not to the first value in the vector delta.
##' ex_out = Example(power_array,
##'                  example = list(delta = 1.0, sd = .7),
##'                  target = .9)
##' ex_out
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
      warning(PrintWrap("The object 'x' you supplied to Example() contains individual iterations. For sensible plotting, these were automatically summarized across simulations using the function given in argument `summary_function`."), call. = FALSE)
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
      target = sse::tex(ex_out_sse, type = 'power'),
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
print.power_example = function(x){
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
  cat(content)
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
summary.power_example = function(x){
  data.frame('example' = unlist(ex_out))
}

