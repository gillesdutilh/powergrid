##' @title Plot Combinations of Parameters for Achieving Target
##' @description Plots the relation between up to three parameters and,
##'   typically, power. Two parameters are represented by x- and y-axes, one by
##'   separate lines.
##' @details In the most typical use case, the y-axis shows the *minimal* sample
##'   size required to achieve a power of *at least* \code{target}, assuming the
##'   value of the parameter on the x-axis, and the value of the parameter
##'   represented by each line.
##'
##' The use of this function is, however, not limited to finding a minimum n to
##' achieve at least a certain power. See help of `Example` to understand the
##' use of `minimal_target` and `fin_min`.
##'
##' Note that a line may stop in a corner of the plotting region, not reaching
##' the margin. This is often natural behavior, when the \code{target} level is
##' not reached anywhere in that corner of the parameter range. In case n is on
##' the y-axis, this may easily be solved by adding larger sample sizes to the
##' grid (consider \code{Update}), and then adjusting the y-limit to only
##' include the values of interest.
##'
##' @param x An object of class "power_array" (from `powergrid`), "power" (from
##'   sse::powEx) or "powCalc" (from sse::powCalc).
##' @param slicer If the parameter grid of `x` has more than 3 dimensions, a
##'   3-dimensional slice must be cut out using \code{slicer}, a list whose
##'   elements define at which values (the list element value) of which
##'   parameter (the list element name) the slice should be cut.
##' @param y_par,x_par,l_par Which parameter is varied on the x- and y-axis, and
##'   between lines, respectively. If NULL, \code{y_par} is set to the first,
##'   \code{x_par} to the second, and \code{l_par} to the third dimension name
##'   of \code{x}.
##' @param par_labels Named vector where elements names represent the parameters
##'   that are plotted, and the values set the desired labels.
##' @param example A list defining for which combination of levels of
##'   \code{l_par} and \code{x_par} an example arrow should be drawn. List
##'   element names indicate the parameter, element value indicate the values at
##'   which the example is drawn.
##' @param target The target power (or any other value stored in x) that should
##'   be matched.
##' @param method The method to find the required parameter values, see
##'   \code{Example} and \code{FindTarget}.
##' @param minimal_target Logical. Should target be minimally achieved (e.g.,
##'   power), or maximially allowed (e.g., estimation uncertainty).
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes an assumption (e.g., minimal required n) to achieve the
##'   \code{target} or an example that maximizes this assumption (e.g.,
##'   maximally allowed SD).
##' @param col A vector with the length of \code{l_par} defining the color(s) of
##'   the lines.
##' @param title Character string, if not \code{NULL}, replaces default figure
##'   title.
##' @param xlim,ylim See \code{?graphics::plot}.
##' @param smooth Logical. If TRUE, a 5th order polynomial is fitted though the
##'   points constituting each line for smoothing.
##' @return A list with graphical information to use in further plotting.
##' @author Gilles Dutilh
##' @examples
## ============================================
## Typical use case: minimal n for power
## ============================================
##' sse_pars = list(
##'   n = seq(from = 2, to = 100, by = 2),
##'   delta = seq(from = 0.1, to = 1.5, by = 0.05), ## effect size
##'   sd = seq(.1, .9, .1)) ## Standard deviation
##' PowFun <- function(n, delta, sd){
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = 0.05)
##'   return(ptt$power)
##' }
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##' GridPlot(power_array, target = .8)
##' ## If that's too many lines, cut out a desired number of slices
##' GridPlot(power_array,
##'          slicer = list(sd = seq(.1, .9, .2)),
##'          target = .8)
##'
##' ## adjust labels, add example
##' GridPlot(power_array, target = .8,
##'          slicer = list(sd = seq(.1, .9, .2)),
##'          y_par = 'n',
##'          x_par = 'delta',
##'          l_par = 'sd',
##'          par_labels = c('n' = 'Sample Size',
##'                         'delta' = 'Arm Difference',
##'                         'sd' = 'Standard Deviation'),
##'          example = list(sd = .7, delta = .6))
##' ## Above, GridPlot used the default: The first dimension is what you search
##' ## (often n), the 2nd and 3rd define the grid of parameters at which the search
##' ## is done. Setting this explicitly, with x, y, and l-par, it looks like:
##' GridPlot(power_array, target = .8,
##'          slicer = list(sd = seq(.1, .9, .2)),
##'          y_par = 'n', # search the smallest n where target is achieved
##'          x_par = 'delta',
##'          l_par = 'sd')
##'
##' ## You may also want to have different parameters on lines and axes:
##' GridPlot(power_array, target = .8,
##'          y_par = 'delta', # search the smallest delta where target is achieved
##'          x_par = 'sd',
##'          l_par = 'n')
##' ## Too many lines! Take some slices again:
##' GridPlot(power_array, target = .8,
##'          slicer = list(n = c(seq(10, 70, 16))),
##'          y_par = 'delta',
##'          x_par = 'sd',
##'          l_par = 'n', method = 'step')
##' @export
GridPlot = function(x,
                          slicer = NULL,
                          y_par = NULL,
                          x_par = NULL,
                          l_par = NULL,
                          par_labels = NULL,
                          example = NULL,
                          target = .8,
                          method = 'step',
                          minimal_target = TRUE,
                          find_min = TRUE,
                          col = grDevices::grey.colors(length(dimnames(x)[[l_par]])),
                          title = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          smooth = FALSE)
{
  ## translator for labels; translates if label is available
  Trans = function(x){
    if(!is.null(par_labels)){
      for(i in seq_along(x)){
        if(x[i] %in% names(par_labels)){x[i] = par_labels[x[i]]}
      }
    }
    return(x)
  }

  ## save current par settings
  old_par <- graphics::par(no.readonly = TRUE)

  ## process input
  x = ArraySlicer(x, slicer)
  if(is.null(y_par))y_par = names(dimnames(x))[1]
  if(is.null(x_par))x_par = names(dimnames(x))[2]
  if(is.null(l_par))l_par = names(dimnames(x))[3]
  ##

  ## order dimensions of input, so that we plot the right dimensions
  ## against eachother. Correct attributes while doing so
  attributes_x = attributes(x)
  x = aperm(x, c(y_par, x_par, l_par))
  attributes_x$dimnames = attributes(x)$dimnames
  attributes_x$dim = attributes(x)$dim
  attributes(x) = attributes_x

  ## find min or max to plot
  y_rec = FindTarget(x, target = target,# min or max required of
                                        # y_par
                     search_par = y_par,
                     find_min = find_min,
                     minimal_target = minimal_target,
                     method = method)
  names(col) = dimnames(x)[[l_par]]
  xvals = as.numeric(dimnames(x)[[x_par]])
  yvals = as.numeric(dimnames(x)[[y_par]])
  if (is.null(xlim)){xlim = range(xvals)}
  if (is.null(ylim)){ylim = range(yvals)}
  ## drawing graph
  graphics::par(las = 1)
  at_x = pretty(xvals[xvals >= xlim[1] & xvals <= xlim[2]])
  at_y = pretty(yvals[yvals >= ylim[1] & yvals <= ylim[2]])
  plot(0,
       xlim = xlim,
       ylim = ylim,
       xlab = Trans(x_par), ylab = Trans(y_par), type = 'n', axes = FALSE)
  graphics::axis(1, at = at_x)
  graphics::axis(2, at = at_y)
  graphics::abline(v = at_x,
                   h = at_y,
                   col = grDevices::grey.colors(1, .95, .95))
  for(i in 1:ncol(y_rec)){
    ys = y_rec[, i]
    xs = as.numeric(dimnames(y_rec)[[x_par]])
    if(smooth){
        plm = stats::lm(ys ~ xs + I(xs^2) + I(xs^3) + I(xs^4) + I(xs^5))
      ys = stats::predict(plm, newdata = data.frame(xs))
    }
    graphics::lines(xs, ys, col = col[i], lwd = 2)}
  graphics::legend('topright', box.lwd = 0, col = col, lwd = 2,
                   legend = names(col),
                   title = Trans(l_par), ncol = min(length(names(col)), 3),
                   bg = 'white', inset = .01)
  ## example
  if(!is.null(example)){
    y_ex = ArraySlicer(y_rec, example)
    usr = graphics::par()$usr
    x0 = grDevices::extendrange(usr[1:2], f = -.02)[1]
    y0 = grDevices::extendrange(usr[3:4], f = -.02)[1]
    graphics::segments(example[[x_par]], y0, example[[x_par]], y_ex)
    graphics::arrows(example[[x_par]], y_ex, x0, y_ex, length = .15)
    graphics::text(x = x0, y = y_ex, labels = y_ex, adj = c(0, -1))
  }
  if(is.null(title)){
    title = paste0(ifelse(find_min, 'Minimum ', 'Maximum '),
                   Trans(y_par),
                   ' for a Power of ',
                   target, '')}
  graphics::title(title)
  ## reset previous par settings
  graphics::par(old_par)
  invisible(list('at_x' = at_x,
                 'at_y' = at_y,
                 'line_colors' = col))
}

