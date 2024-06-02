##' Find combination of parameters yielding desired power (or other value) in an
##' object of class \code{power_array}.
##'
##' In the most common use case, and this is also the default, \code{Example}
##' searches the *minimal* n where the power is *at least* equal to the value
##' given by argument \code{target}. More generically, the object x, sliced at
##' the values given in argument \code{example} results in a vector. This vector
##' is searched for the *minimal* value of the names of the vector where the
##' value of the vector is *at least* equal to 'code{target}. Thus, when the
##' example is a vector of power at different effect sizes, it searches the
##' minimal effect size at which the target power is achieved.
##'
##' Above, *minimal* is defined by the default value TRUE of argument
##' \code{find_min}. If \code{find_min} is FALSE, the maximum is searched. This
##' is useful in the situation where one searches for the highest standard
##' deviation at which it is still possible to find a desirable power.
##'
##' Above, *at least* is defined by the default value TRUE
##' argument\code{minimal_target}. Setting \code{minimal_target} to FALSE allows
##' to search, for example, for the minimal sample size where the expected
##' confidence interval is smaller than a certain desired width.
##'
##' @title Find Combination of Parameters Yielding Desired Power.
##' @param x Object of class \code{power_array} or \code{power}
##' @param example List with named elements pointing at the assumptions at which
##'   the example should be based. List names should match the dimension names
##'   of \code{x}, assumptions should be exact values at these dimensions.
##' @param target Which value should be achieved at the example
##' @param minimal_target Logical. Should target be minimally achieved (e.g.,
##'   power), or maximially allowed (e.g., estimation uncertainty).
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes an assumption (e.g., minimal required n) to achieve the
##'   \code{target} or maximizes this assumption (e.g., maximally allows SD).
##' @param method Character string, indicating how the location of the example
##'   is found, as implemented in \code{FindTarget}. Either \code{"step"}:
##'   walking in steps along \code{search_par} or \code{"lm"}: Interpolating
##'   assuming a linear relation between \code{search_par} and (qnorm(x) +
##'   qnorm(1 - 0.05)) ^ 2. Setting 'lm' is inspired on the implementation in
##'   the sse package by Thomas Fabbro.
##' @return Returns a list containing:
##'
##' - requested_example: the parameters at
##'   which the power was evaluated to search n
##'
##' - required_value: the minimum (or maximum if slot searched = 'max') for n
##' (or other required_name)
##'
##' - required_name: the parameter minimized (or maximized if slot searched =
##' 'max')
##'
##' - searched: was the "min" or "max" <required_name> searched?
##'
##' - objective: was <required_name> searched to find the "min" or "max" of x.
##'
##' - target: which value should the power be
##'
##' - minimal_target: At minimum (TRUE) or maximum (FALSE)
##' @author Gilles Dutilh
##' @examples
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
##' ##
##'
##' ex_out = Example(power_array,
##'                  example = list(delta = .7, sd = .7),
##'                  target = .9)
##' ex_out
Example = function(x,
                   example = NULL,
                   target = NULL,
                   minimal_target = TRUE,
                   find_min = TRUE,
                   method = 'step'){
  if (method == 'lm' && any(target %in% 0:1)){
    stop("Method is set to 'lm', which only makes sense for power as a function of n. Searching for a power of 1 or 0 is not supported by this package. For help achieving a power of 1 or 0, see a priest or a shrink, respectively.")}
  if(inherits(x, 'power')) {
    ## translate info from powEx output (class: power) Example() output
    requested_example = list(theta = sse::tex(x, type = 'theta'))
    required_value = sse::tex(x, type = 'nRec')
    required_name = 'n'
    ## at_value = sse::tex(x, type = 'theta')
    ## at_name = 'theta'
    example_list = list(requested_example = requested_example,
                        required_value = required_value,
                        required_name = required_name,
                        searched = 'min',
                        objective = 'min')
    ## at_value = at_value,
    ## at_name = at_name)
  } else {
    ## use Example to take the example from a desired onedimensional slice (vector)
    requested_example = example
    slice_to_search = ArraySlicer(x, example)
    required_value = FindTarget(slice_to_search, target = target,
                                minimal_target = minimal_target,
                                search_par = 'this is ignored for vector',
                                find_min = find_min,
                                method = method)
    required_name = attr(slice_to_search, which = 'dims_left')
    ## required_name = names(dimnames(slice_to_search))
    ## x_ex_name = names(margins_toplot)[names(margins_toplot) != par_to_search]
    ## x_ex_value = example[[x_ex_name]]
    ## note that "y_ex_name" is not defined, this is par_to_search
    example_list = list(requested_example = requested_example,
                        required_value = required_value,
                        required_name = required_name,
                        searched = ifelse(find_min, 'min', 'max'),
                        objective = ifelse(minimal_target, 'min', 'max'),
                        target = target,
                        method = method)
  }
  if (method == 'step' & is.na(required_value)){
    warning(paste0('No value of ', required_name, ' evaluated where target of ',
                   target,
                   ' was achieved.\nConsider using method = "lm" for extrapolation.'))
    }
  class(example_list) = 'power_example'
  return(example_list)
}

##' Print method for class \code{power_example}.
##'
##' Print short informative output for object of class \code{power_example}.
##'
##' @title Print Example
##' @param x object of class \code{power_example}
##' @return nothing
##' @author Gilles Dutilh
print.sse_example = function(x){
  cat('================================================\n')
  cat(paste0('For a ', ifelse(x$objective == 'min', 'minimal', 'maximal'), ' power of ',
             x$target, ' assuming\n',
             paste(names(x$requested_example),
                   x$requested_example, sep = ' = ', collapse = '\n'), '\n',
             ifelse(x$searched == 'min', 'Minimal required ', 'Maximal persmissible '),
             x$required_name, ' = ', x$required_value,
             ifelse(x$method == 'lm',
                    '\nMethod "lm" was used to approach the required n,
which makes sense only for studying the relation
between n and power.',
                    '')
             )
      )
  cat('\n================================================\n')
}
