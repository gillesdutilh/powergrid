##' @title Scale to desired range
##' @description Linear scaling of a vector to a vector with a required range.
##' @param x A vector
##' @param a The desired minimum
##' @param b The desired maxmum
##' @return a vector with the desired minimum and maximum
##' @author Gilles Dutilh
ScaleRange = function(x, a, b){
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}

##' @title Wrap text in printed output.
##' @description Wrap text in printed output. This function is created for
##'   convenience and also so that the width of wrapping can be set by simply
##'   changing the default here. Note that this give a leading white space to
##'   each resulting line. The is done to account for the fact that wrapping
##'   deletes the whites paces, and it actually looks good.
##' @param x A single character string
##' @param width The desired width
##' @param ... Further arguments passed to 'strwrap()'
##' @return a character string with returns placed such to achieve the desired
##'   width
##' @author Gilles Dutilh
PrintWrap = function(x,
                     width = 0.75 * getOption("width"), ...){
  if (x == ''){
    x
  } else {
    paste0(' ', paste0(strwrap(x, width = width), collapse = '\n '))
  }
}

##' @title Summary of object that has simulations saved.
##' @description Summarizes objects of class `power_array` that have individual
##'   simulations saved across simulations.
##' @param x Object of class `power_array`
##' @param summary_fun function to apply across simulations
##' @param ... Further arguments passed to 'summary_fun'
##' @return An object of class `power_array`, with attributes \code{summarized =
##'   TRUE}.
##' @author Gilles Dutilh
PowerApply = function(x, summary_function, ...){
  if(attr(x, which = 'summarized') | class(x) != 'power_array'){
    stop('Object x should be an object of class `power_array`, where attribute `summarized` is FALSE; containing individual simulations.')
  }
  aa = attributes(x)
  summarized_x = apply(x, names(dimnames(x))[names(dimnames(x)) != 'sim'],
                       summary_function, ...)
  new_attributes = attributes(summarized_x)
  for (cur_attribute in names(aa)[!(names(aa) %in% c('dim', 'dimnames'))])
    { # copy attributes
      new_attributes[[cur_attribute]] = aa[[cur_attribute]]
    }
  ## change summary-related attributes
  new_attributes$summarized = TRUE
  new_attributes$summary_function = summary_function
  new_attributes$summary_function_name =
    ifelse (class(substitute(summary_function)) == 'name',
            substitute(summary_function),
            ## if created on the fly, it's an ananymous function
            "anonymous function"
            )
  attributes(summarized_x) = new_attributes
  return(summarized_x)
}


