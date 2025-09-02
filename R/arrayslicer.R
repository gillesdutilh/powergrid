##' @title Cut slice from array (typically of class `power_array`)
##' @description Cut out a slice from an array. The resulting slice may be
##'   single- or multidimensional. The function is intended for arrays of class
##'   "power_array", and makes sure that the resulting array is of class
##'   power_array and keeps and, where needed, updates the object's
##'   attributes. These attributes are needed for various functions in the
##'   powergrid package to work well.
##' @details Internally, indexing ([) is used, but the implementation in
##'   ArraySlicer is very flexible allowing for any number of dimensions in any
##'   order in the \code{slicer} argument. The resulting slice is always an
##'   array, also if only one dimension is left. \code{dimnames} are kept
##'   intact.
##' @param x An array, in most common use cases an array of class
##'   \code{power_array}, but may be any array with named dimensions.
##' @param slicer A list whose named elements define at which dimension (the
##'   list element names), at which values (the list element values) a slice is
##'   taken from \code{power_array}. Default NULL returns the unchanged array.
##' @return An array with reduced dimensions as given by \code{slicer}. Note
##'   that, relative to a standard array, some additional attributes are passed
##'   to be used in the functions in package \code{powergrid}
##' @author Gilles Dutilh
##' @seealso \code{\link{PowerGrid}}, `[.power_array` for reducing the dimensions of
##'   an array of class `power_array` using [-indexing.
##' @examples
##' sse_pars = list(
##'   n = seq(from = 20, to = 60, by = 5),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.2),
##'   sd = seq(.1, .9, .2),
##'   alpha = c(.05, .025, .1)) # a 4-dimensional grid
##' PowFun <- function(n, delta, sd, alpha){
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = alpha)
##'   return(ptt$power)
##' }
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##' ## cut out a 2-dimensional plane:
##' ArraySlicer(power_array,
##'             slicer = list(alpha = .1, sd = .9))
##' ## Note that above, the dimension levels are called as numeric values, so the
##' ## following works as well:
##' ArraySlicer(power_array,
##'             slicer = list(alpha = 0.1, sd = 0.9))
##' ## They can be called by their actual character values as well:
##' ArraySlicer(power_array,
##'             slicer = list(alpha = '0.1', sd = '0.9'))
##' ## (compare with dimnames(power_array))
##' ## the following does not work:
##'\dontrun{
##' ArraySlicer(power_array,
##'             slicer = list(alpha = '.1', sd = '.9'))
##'}
##' ##
##' ## Cut out multiple levels from one dimension
##' ArraySlicer(power_array,
##'             slicer = list(alpha = .1, sd = c(.9, .7)))
##' @export
ArraySlicer = function(x, slicer = NULL)
{
  dimnms = dimnames(x)
  ## input feedback
  if(!all(names(slicer) %in% names(dimnms))){
    no_el = names(slicer)[!(names(slicer) %in% names(dimnms))]
    stop(paste0("Element", ifelse(length(no_el) == 1, " '", "s '"),
                paste0(no_el, collapse = "' and '"),
                "' in slicer ",
                ifelse(length(no_el) == 1, "does", "do"),
                " not occur in dimension names of input array"))
  }
  for(i in names(slicer)){
    if(!(all(slicer[[i]] %in% dimnms[[i]]))){
      stop(paste0("Slicer element '", i,
                  "' contains a value that does not occur in dimension '",
                  i,
                  "' of x"))
    }
  }
  ## Because we cannot assume that slicer has the right order of
  ## arguments, we need to loop over the dimensions of x (or
  ## find a better solution later).
  index_list = dimnms # index_list will contain the desired dimnms
  for(cur_dim in names(dimnms)){
    if(cur_dim %in% names(slicer)){
      index_list[[cur_dim]] = as.character(slicer[[cur_dim]])
    }
  }
  ## translate index_list to index string to be used as index in parse.
  index_string =
    paste(lapply(index_list,
                 function(x){
                     ifelse(is.null(x),
                            '', # is null, for sim dimension
                            paste0("c('", paste0(x, collapse = "', '"), "')"))}
                 ),
          collapse = ', ')
  slice = eval(parse(text = paste0("x[", index_string, "]")))
  ## Deal with one-dimensional output
  index_lengths = sapply(index_list, length)
  if(sum(index_lengths > 1) == 1) # that is, only a vector left in slice
  { # give names (they are used in FindTarget
    slice = as.array(slice)
    dimnames(slice) = index_list[names(index_lengths[index_lengths > 1])]
  }
  attr(slice, which = "sliced_at") = slicer
  attr(slice, which = "dims_left") = names(dimnms)[!(names(dimnms) %in% names(slicer))]
  ## this attribute is only used in Example
  return(slice)
}

