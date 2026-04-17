##' @title Check the dimensions of a power array
##'
##' @param x A power array
##' @param required_dim The number of dimensions required. If NULL just returns
##' the number of dimensions.
##' @param condition If required dimensions are not correct should a `warning` or
##' `error` be produced. Ignored if required dimensions is NULL.
##' @param sliced For the condition text should the user be informed the array
##' is sliced. Ignored if required dimensions is NULL.
##'
##' @returns An integer for the number of dimensions
CheckArrayDim =
  function(x, required_dim = NULL, condition = "warning", sliced = FALSE) {

    dim <- length(dim(x))

    ## TODO: I don't think this is necessary, an array always has dimension 1
    ## unless you try really hard to make an empty array.
    if (dim == 0){
      dim = ifelse(length(x) > 0, 1, 0)
    }

    if(!is.null(required_dim) && !dim %in% required_dim) {

      condition_text <- paste0("The input ", ifelse(sliced, "(after slicing) ", ""),
      "does not define an array with dimension equal to one of {",
      paste0(required_dim, collapse = ', '),"}, as expected. ",
      "Instead the array has dimensions of ", dim, ".")

      if(condition == "error") stop(condition_text)
      if(condition == "warning") warning(condition_text)
    }

    return(dim)
  }

##' @title Ensure a power_array is summarized
##'
##' @param x A power array
##' @param summary_function What function should be used to summarize the array,
##' if it is not already summarized.
##' @param condition If array is not summarised should a `warning` or
##' `error` be produced. Ignored if required dimensions is NULL.
##' @returns A summarised power_array
EnsureSummarized = function(x, summary_function = NULL, condition = "warning") {
  if(!attr(x, which = 'summarized')){
    if(condition == "error") {
      stop(paste0(
        "The object 'x' you supplied to unexpectedly contains individual ",
        "iterations."), call. = FALSE)
    }
    if(condition == "warning") {
      warning(paste0(
        "The power array you supplied to contains individual ",
        "iterations. To be used further these were automatically ",
        "summarized across iterations using the provided summary function"),
        call. = FALSE, immediate. = TRUE)
    }
    if(is.null(summary_function)) stop("No summary function provided", call. = TRUE)

    x = SummarizeIterations(x, summary_function)
  }
  return(x)
}

##' @title Ensure a power_array has only a single fun_out value
##'
##' @description If multiple fun_out values are present in the input take the first one.
##'
##' @param x A power array
##' @param condition If array is not summarised should a `warning` or
##' `error` be produced. Ignored if required dimensions is NULL.
##' @param sliced For the condition text should the user be informed the array
##' is sliced. Ignored if required dimensions is NULL.
##'
##' @returns A power array with only a single fun_out value
EnsureSingleFunOut <- function(x, condition = "warning", sliced = FALSE) {

  if (attr(x, 'sim_function_nval') > 1)
  {
    if(condition == "error") {
      stop(paste0(
        "The power array you supplied contains multiple function outputs at each parameter combination ",
        ifelse(sliced, "(after slicing) ", ""),"."
      )
      )
    }

    chosen_fun_out = attr(x, 'dimnames')$fun_out[1]
    x = ArraySlicer(x, slicer = list(fun_out = chosen_fun_out))

    if(condition == "warning") {
      warning(paste0(
        "The power array you supplied contains multiple function outputs at each parameter combination ",
        ifelse(sliced, "(after slicing) ", ""),". \n*** Function output ",
        chosen_fun_out,
        " was automatically chosen to be plotted! ***\nTo explicitly choose a function ",
        "output, do so using argument 'slicer', including 'fun_out = <output name> in that list."
      ), call. = FALSE)
    }

  }
  return(x)
}

