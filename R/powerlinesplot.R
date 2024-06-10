##' @title Plot Combinations of Parameters for Achieving Target Powerlevel
##' @description Plots the relation between up to three parameters and
##'   power.
##' @details Assuming the value of the parameter on the x-axis, and the value of
##'   the parameter represented by each line line, the y-axis shows the
##'   *minimal* sample size required to achieve a power of *at least*
##'   \code{target}. The former defines the default behavior which reflects the
##'   most common use case. However, above, *minimal* and *at least* are
##'   governed by two arguments that allow for more flexible plotting:
##'
##' First, *minimal* is defined by the default value TRUE of argument
##' \code{find_min}. If \code{find_min} is FALSE, the maximum is searched. This
##' is useful in the situation where one searches for the highest standard
##' deviation at which it is still possible to find a desirable power.
##'
##' Second, *at least* is defined by the default value TRUE
##' argument\code{minimal_target}. Setting \code{minimal_target} to FALSE allows
##' to search, for example, for the minimal sample size where the expected
##' confidence interval is smaller than a certain desired width.
##'
##' Note that a line may stop in a corner of the plotting region. This is often
##' natural behavior, when the \code{target} level is not reached anywhere in
##' that corner of the parameter range. In case n is on the y-axis, this may
##' easily be solved by adding larger sample sizes to the grid (consider
##' \code{Update}), and then adjusting the y-limit to only include the values of
##' interest.
##'
##' @param x An object of class `power_array' (from powergrid), "power" (from
##'   sse::powEx) or `powCalc' (from sse::powCalc). Other arrays or matrices may
##'   work as well.
##' @param slicer If the parameter grid for which `x' was constructed has more
##'   than 3 dimensions, a 3-dimensional slice may be cut out using
##'   \code{slicer}, a list whose elements define at which values (the list
##'   element value) of which parameter (the list element name) the slice should
##'   be cut.
##' @param y_par,x_par,l_par Which parameter is varied on the x- and y-axis, and
##'   between lines, respectively. If NULL, \code{y_par} is set to the first,
##'   \code{x_par} to the second, and \code{l_par} to the third dimension name
##'   of \code{x}.
##' @param par_labels Named vector with elements named as the parameters
##'   plotted, with as values the desired labels.
##' @param example A list defining for which combination of levels of
##'   \code{l_par} and \code{x_par} an example arrow should be drawn. List
##'   element names indicate the parameter, element value indicate the values at
##'   which the example is drawn.
##' @param target The target power (or any other value stored in the
##'   power_array) that should be matched
##' @param method The method to find the required parameter values, see
##'   \code{Example} and \code{FindTarget}.
##' @param minimal_target Logical. Should target be minimally achieved (e.g.,
##'   power), or maximially allowed (e.g., estimation uncertainty).
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes an assumption (e.g., minimal required n) to achieve the
##'   \code{target} or an example that maximizes this assumption (e.g.,
##'   maximally allows SD).
##' @param col A vector with the length of \code{l_par} defining the color(s) of
##'   the lines.
##' @param title Character string, if not \code{NULL}, replaces default figure
##'   title.
##' @param xlim,ylim See \code{?graphics::plot}.
##' @param smooth Logical. If TRUE, a 5th order polynomial is fitted though the
##'   points constituting each line for smoothing.
##' @return
##' @author
PlotPowerLines = function(x,
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
  return(list('at_x' = at_x,
              'at_y' = at_y,
              'line_colors' = col))
}

