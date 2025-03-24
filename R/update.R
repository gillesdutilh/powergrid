## This function offers two functionalities with respect to power_array
## objects:
##
## 1) to perform additional iterations (only for power_array where attr
## keep_sims = TRUE)
##
## 2) to evaluate a function on additional locations of the parameter
## grid.

##' @title Refine or Extend Power Array
##' @description Add further results to an existing power_array (created by
##'   PowerGrid or by another call of Update), adding further values in
##'   \code{pars} and/or by larger \code{n_iter}.
##' @details If \code{pars == NULL}, update extends \code{old} by adding
##'   iterations \code{n_iter_add} to the existing power_array. If \code{pars}
##'   is given, the function that was evaluated in \code{old} (attribute
##'   \code{sim_function}) is evaluated at the crossings of \code{pars}. If
##'   argument \code{pars} is different from \code{attr(old, which = 'pars')},
##'   this means that the function is evaluated additional crossings of
##'   parameters.
##'
##'   Note that certain combinations of \code{pars} and \code{n_iter_add} result
##'   in arrays where some crossings of parameters include more iterations than
##'   others. This is a feature, not a bug. May result in less aesthetic
##'   plotting, however.
##' @param old the object of class `power_array` to extend
##' @param n_iter_add the number of iterations to *add* to old
##' @param pars the new parameter grid to evaluate across
##' @param ... further arguments passed on to FillGrid internally.
##' @return object of class `power_array`, containing old, extended by
##'   \code{pars} and/or \code{n_iter_add}.
##' @author Gilles Dutilh
##' @examples
##' ## very simple example with one parameter THAT DOES NOT WORK
##' ## pars = list(x = 1:2)
##' ## fun = function(x){round(x+runif(1, 0, .2), 3)}
##' ## original = FillGrid(pars = pars,
##' ##                     fun = fun,
##' ##                     n_iter = 3,
##' ##                     summarize = FALSE)
##'
##' ## example of both adding samples (for pars x = 2),
##' ## for leaving one par value out (x = 0)
##' ## or adding one additional par value (x = 3)
##' ## original
##' ## updated = Update(original, n_iter_add = 2, pars = list(x = 2:3))
##' ## updated
##' ## attributes(updated)
##'
##' ## example with 2 parameters
##' pars = list(x = 1:2, y = 1:2)
##' fun = function(x, y){round(x * 10 + y + runif(1, 0, .2), 3)}
##' original = FillGrid(pars = pars,
##'                     fun = fun,
##'                     n_iter = 3,
##'                     summarize = FALSE)
##' updated = Update(original, n_iter_add = 2, pars = list(x = 2:3, y = 4:5))
##'
##' ## ## example with 2 parameters, two-outcome-function THAT DOES NOT WORK
##' ## pars = list(x = 1:2, y = 1:2)
##' ## fun = function(x, y){
##' ##   c('bla' = round(x * 10 + y + runif(1, 0, .2), 3),
##' ##   'bli' = sum(x, y))}
##' ## original = FillGrid(pars = pars,
##' ##                     fun = fun,
##' ##                     n_iter = 3,
##' ##                     summarize = FALSE)
##' ## updated = Update(original, n_iter_add = 2, pars = list(x = 2:3, y = 4:5))
##' ## updated[, ,'bli', ]

Update = function(old, n_iter_add = 1, pars = NULL, ...){
  if (is.null(pars)) {pars = attr(old, 'pars')}
  ## copy the original attributes to add later
  copy_attr = attributes(old)
  ## number of zeros later needed for iter-dimension
  nzeros = max(ceiling(log(n_iter_add, 10)),
               ceiling(log(attr(old, 'n_iter'), 10)))
  ## Perform simulation in grid, using, where relevant, the attributes
  ## of old.
  new = FillGrid(pars = pars,
                 fun = copy_attr$sim_function,
                 n_iter = n_iter_add,
                 more_args = copy_attr$more_args,
                 summarize = FALSE, ...)
  ## =================================
  ## The next chunk combines the old and new arrays into an array,
  ## thereby taking care that the result has missing values for eventual
  ## combinations of pars that were not evaluated in old.
  ##
  ## I use ftable to turn old and new array into grid, to correctly
  ## combine before transforming them back into an array. Note that
  ## ftable gives some funny colnaming, which I correct (rename letters
  ## to integer)
  ##
  ## Note that I reuse object name to save memory space, making code much
  ## less easy to read.
  old = stats::ftable(old, col.vars = 'sim') # flat table
  attr(old, 'col.vars')$sim = as.character(1:ncol(old))
  old = as.data.frame(old) # expanded grid pars * sims
  new = stats::ftable(new, col.vars = 'sim') # flat table
  attr(new, 'col.vars')$sim = as.character(1:ncol(new))
  new = as.data.frame(new)# expanded grid pars * sims
  ## Labels for sims need unique names between sets
  old$sim = paste0('old_', sprintf(paste0('%0', nzeros, '.0f'), old$sim))
  new$sim = paste0('upd_', sprintf(paste0('%0', nzeros, '.0f'), new$sim))
  new = rbind(old, new) # new now contains old as well!
  ##
  ## If pars (new) are different than pars (old) make a grid with all
  ## the possible combinations, including the ones never
  ## substantiated. Call is complicated due to memory-saving reusing of
  ## object names. For understanding, I kept the commented line below
  ## which creates exp_grid_hypo, which is then merged.
  ##
  ## exp_grid_hypo = expand.grid(lapply( exp_grid_combi[,
  ## names(exp_grid_combi) != 'Freq'], unique))
  ## !! I reuse the name 'old' below, to contain the merged version !!
  ## I do so, because I need the long version later and old's use is gone.
  old = merge(expand.grid(lapply(
    new[, names(new) != 'Freq'], unique)),
    new, all.x = TRUE)
  ## change back grid values to numeric:
  old[, seq_along(length(pars))] = sapply(old[, seq_along(length(pars))],
                                 function(x)as.numeric(as.character(x)))
  ## sort (in a rather complicated way, since we do not know the number
  ## of columns):
  old = old[do.call(order, as.data.frame(as.matrix(old[, -ncol(old)]))), ]
                                        # to have the correct grid point names when using xtabs, change from num/fac to character
  old[, -ncol(old)] = apply(old[, -ncol(old)], 2, as.character)
  new = stats::xtabs(Freq ~ ., data = old)
  new[as.matrix(old[is.na(old$Freq), names(old) != 'Freq'])] = NA
  dimnames(new)[-length(dimnames(new))] =
    lapply(dimnames(new)[-length(dimnames(new))],
           function(x)as.numeric(as.character(x)))
  ##
  ## =================================
  attr(new, 'call') = NULL # remove irrelevant attribute
  ## change the changed attributes
  copy_attr$dim = dim(new)
  copy_attr$dimnames = dimnames(new)
  copy_attr$n_iter = copy_attr$n_iter + n_iter_add
  ## set attributes
  attributes(new) = copy_attr
  return(new)
}
