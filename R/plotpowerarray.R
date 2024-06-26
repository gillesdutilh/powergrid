##' @title Create Power Plot
##' @description Plot (a slice of) an object of class \code{power_array}. Main
##'   purpose is to illustrate the relation between two parameters (e.g., effect
##'   size on the x-axis and n on the y-axis) for a given target power. An
##'   example may be highlighted by drawing an arrow at the combination of
##'   parameters deemed most likely.
##' @details The most common use case may be plotting the required n (on the
##'   y-axis) as a function of some other parameter (e.g., effect size, on the
##'   x-axis) for achieving a certain level of statistical power. The default
##'   argument settings reflect this use case.
##'
##'   The plotting is, however, more flexible. For example, one may flip the
##'   axes by setting a different \code{par_to_search} (which defines the
##'   y-axis). One may also search not the minimum, as in the case of sample
##'   size, but the maximum, e.g., the highest sd at which a certain power may
##'   still be achieved. In this case, the \code{par_to_search} is sd, and
##'   \code{find_min = FALSE}.
##'
##'   Further, in the classic use case, one aims at a *minimal* level of
##'   power. One may however also aim at, e.g., a *maximal* width of a
##'   confidence interval. For this purpose, set \code{minimal_target} to
##'   \code{FALSE}.
##' @param x An object of class `power_array' (from powergrid), "power" (from
##'   sse::powEx) or `powCalc' (from sse::powCalc), or just any 2-dimensional
##'   array or matrix.
##' @param slicer If the parameter grid for which `x' was constructed has more
##'   than 2 dimensions, a 2-dimensional slice may be cut out using
##'   \code{slicer}, a list whose elements define at which values (the list
##'   element value) of which parameter (the list element name) the slice should
##'   be cut.
##' @param par_to_search The variable whose minimum (or maximum, when
##'   \code{find_min == FALSE}) is searched for achieving the
##'   \code{target_levels}.
##' @param find_min Logical, indicating whether the example should be found that
##'   minimizes an assumption (e.g., minimal required n) to achieve the
##'   \code{target} or an example that maximizes this assumption (e.g.,
##'   maximally allows SD).
##' @param example If not NULL, a list of length one, defining at which value
##'   (list element value) of which parameter (list element name) the example is
##'   drawn for a power of \code{target}. (see \code{?Example} for details).
##' @param method Method used for finding the required \code{search_par} needed
##'   to achieve \code{target}. Either \code{'step'}: walking in steps along
##'   \code{search_par} or \code{'lm'}: Interpolating assuming a linear relation
##'   between \code{search_par} and \code{(qnorm(x) + qnorm(1 - 0.05)) ^
##'   2}. Setting 'lm' is inspired on the implementation in the \code{sse}
##'   package by Thomas Fabbro.
##' @param target The power (or any other value) for which the example, if
##'   requested, is drawn. Also defines which of the power lines is drawn with a
##'   thinker line width.
##' @param minimal_target Logical. Should target be minimally achieved (e.g.,
##'   power), or maximially allowed (e.g., estimation uncertainty).
##' @param summary_function If \code{x} is an object of class \code{power_array}
##'   where attribute \code{summarized} is FALSE (and individual iterations are
##'   stored in dimension \code{sim}, the iterations dimension is aggregated by
##'   \çode{summary_fun}. Otherwise ignored.
##' @param target_levels For which levels of power (or any other variable) are
##'   lines drawn.
##' @param shades_of_grey Logical indicating whether greylevels are painted in
##'   addition to isolines to show power levels.
##' @param shades_legend Logical indicating whether a legend for the shading is
##'   added. Note that this legend is drawn in a separate plotting region, and
##'   does effect setting \code{par(mfrow)}.
##' @param title Character string, if not \code{NULL}, replaces default figure
##'   title.
##' @param par_labels Named vector with elements named as the parameters
##'   plotted, with as values the desired labels.
##' @param smooth Numeric, defaults to NA, meaning no smoothing. Non NA value is
##'   used as argument \code{span} for smoothing with \code{stats::loess},
##'   regressing the contour values on the x and y-axis. Suggested value is
##'   .35. Functionality implemented for consistency with \code{sse} package,
##'   but use is discouraged, since regressing the contour values flattens the
##'   contour plot, thereby biasing the contour lines.
##' @param ... Further arguments are passed on to function `image`
##'   internally. Most useful for zooming with xlim and ylim.

##' @param ...
##' @return
##' @author
PlotPower =
    function(x, # power_array or powEx output (class = power)
             slicer = NULL, # which plain of the grid
             par_to_search = 'n', # default, for what should we find the min/max
             find_min = TRUE, # search for min or max in par_to_search
             example = NULL, # a list(<parameter> = <value>)
             method = 'step',
             target = .9, # the minimum (or maximum, see below)
             minimal_target = TRUE,
             summary_function = function(x){mean(x, na.rm = TRUE)},
             target_levels = c(.8, .9, .95), # which power iso lines to draw
             shades_of_grey = TRUE, # do you want shades of grey on background
             shades_legend = FALSE, # do you want a legend for the shades
             title = NULL,
             par_labels = NULL,
             smooth = NA,
             ...) # dictionary vector of <varname> = <varlabel>
{
  ## =======================================================
  ## process input
  ## =======================================================
  ##
  ## ============================================
  ## handle input from old sse package
  if(all(class(x) == 'power')){
    power_array = drop(GetPowergrid(x))
    example = list(theta = sse::tex(x, type = 'theta'))
    target = x@power.example
    slicer = list(xi = x@xi.example)
  } else if(all(class(x) == 'powCalc')){
    power_array = drop(GetPowergrid(x))
    ## handle PowerGrid input
  } else if (all(class(x) == 'power_array')){
    copy_attr = attributes(x)
    if(!attr(x, which = 'summarized')){ # iterations kept
      warning(paste0(strwrap('Note that the object in argument `x` is summarized across iterations by function passed in argument `summary_function`.',
                             60), '\n'))
      if(any(is.na(x))){cat('Some of the cells of input array (after eventual slicing) were empty.\n')}
      power_array = apply(x, 1:(length(dim(x)) - 1), summary_function)
      copy_attr$dim = dim(power_array)
      copy_attr$dimnames = dimnames(power_array)
      copy_attr$summarized = TRUE
      copy_attr$summary_function = summary_function
      attributes(power_array) = copy_attr
    } else {
      power_array = x # power_array
    }
  } else if (is.array(x)){
    power_array = x} # if just any array

  ## ============================================
  ## take slice that should be plotted
  if(!is.null(slicer)){
    array_toplot = ArraySlicer(power_array = power_array, slicer = slicer)
  } else {array_toplot = power_array}

  ## feedback if the number of dimension are not correct
  left_dims = length(dim(array_toplot))
  if (left_dims == 0){
    left_dims = ifelse(length(array_toplot) > 0,
                       1, 0)
  }
  if(left_dims != 2){
    stop(paste0(strwrap(
      paste0(
        ifelse(is.null(slicer),
               "Input 'x' was no 2-dimensionsonal array, ",
               "Slicing 'x' by 'slicer' did not yield the necessary 2-dimensional, "),
        "but a ", left_dims, "-dimensional array instead."), 75), collapse = '\n'))
  } ##

  dimnms = names(dimnames(array_toplot))
  dimorder = c(par_to_search, dimnms[dimnms != par_to_search])
  array_toplot = aperm(array_toplot, dimorder)
  margins_toplot = dimnames(array_toplot) # what are the values on the axes
  ##
  ## =======================================================
  ## Graphical preparation
  ## =======================================================
  ##
  ## ============================================
  ## Calculate colors and legend values if shades_of_grey
  if(shades_of_grey){
    n_breaks = 101 # granularity of colors, more is better, but 101 is enough
    ## calculate breaks, which are used to define legend colors.
    breaks = seq(0, 1, length = n_breaks)
    image_cols = grDevices::grey.colors(length(breaks) - 1, start = 0.3, end = .9)
    legend_ats = pretty(array_toplot)
    legend_cols = image_cols[
      cut(legend_ats, breaks, labels = 1:(n_breaks-1), include.lowest = TRUE)]
  } else
  {image_cols = grDevices::grey.colors(1, .9, .9)}
  ##
  ## ============================================
  ## translator for labels; translates if label is available
  Trans = function(x){
    if(!is.null(par_labels)){
      for(i in seq_along(length(x))){
        if(x[i] %in% names(par_labels)){x[i] = par_labels[x[i]]}
      }
    }
    return(x)
  }
  ## texts
  slice_at = slicer[lapply(slicer, length) == 1] # I construct this
                                        # to deal with
                                        # cases where
                                        # dimensions are
                                        # only reduced by
                                        # slicer, not cut
                                        # out.
  if (is.null(title)){
    title = ifelse(is.null(slicer) |
                   all(names(dimnames(array_toplot)) == names(slicer))
                 , "Power",
                   paste('Power at',
                         paste(Trans(names(slice_at)), '=', slice_at,
                               collapse = ', ')))
  }
  ## =======================================================
  ## Draw figure
  ## =======================================================
  ## ============================================
  ## Legend plot if needed (shades of grey & shades legend are requested)
  ## is a separate plotting region
  if(shades_of_grey && shades_legend) {
    graphics::layout(t(2:1), widths = c(5, 1)) # in this order, so that
                                        # you can edit the main fig afterwards
    graphics::par(mar = c(10, 1, 10, 3))
    graphics::image(1, seq_along(legend_ats), t(rev(legend_ats)),
          axes = FALSE, xlab = '', ylab = '', col = rev(legend_cols))
    graphics::text(1, seq_along(legend_ats),
                   labels = sprintf('%1.1f', legend_ats),
         cex = 1.5, col = grDevices::grey.colors(1, .2, .2))
    graphics::mtext(side = 3, line = 2, text = 'Power')
  }
  ## ============================================
  ## Main plot.
  ## Image contains shades of grey or white, creating higher level plot
  graphics::par(las = 1, mar = c(5.1, 4.1, 4.1, 2.1))
  graphics::image(as.numeric(margins_toplot[[2]]),
        as.numeric(margins_toplot[[1]]),
        t(array_toplot),
        ylab = Trans(names(margins_toplot)[[1]]),
        xlab = Trans(names(margins_toplot)[[2]]),
        axes = FALSE, col = image_cols, main = title, ...)
  ## XXX below is work on the legend inside the plot.
  ## axis(1)
  ## axis(1, at = margins_toplot[[2]], line = 2)
  ## num_margins_toplot = lapply(margins_toplot, as.numeric)
  ## image_x_lim_min = num_margins_toplot[[2]][1] -
  ##   (num_margins_toplot[[2]][2] - num_margins_toplot[[2]][1])/2
  ## image_x_lim_max = rev(num_margins_toplot[[2]])[1] +
  ##   (rev(num_margins_toplot[[2]])[1] - rev(num_margins_toplot[[2]])[2])/2
  ## xlim = c(image_x_lim_min,
  ##          image_x_lim_min + (image_x_lim_max - image_x_lim_min) * 1.2)
  ## graphics::image(as.numeric(margins_toplot[[2]]),
  ##       as.numeric(margins_toplot[[1]]),
  ##       t(array_toplot),
  ##       ylab = Trans(names(margins_toplot)[[1]]),
  ##       xlab = Trans(names(margins_toplot)[[2]]),
  ##       axes = FALSE, col = image_cols, main = main,
  ##       xlim = xlim)
  ## axis(1);axis(2)
  ## legend_x = mean(c(image_x_lim_max, xlim[2]))
  ## y_range = extendrange(as.numeric(margins_toplot[[1]]), f = -0.15)
  ## legend_y = ScaleRange(legend_ats, y_range[1], y_range[2])
  ## graphics::image(legend_x, legend_y, t(rev(legend_ats)), add = TRUE,
  ##       axes = FALSE, xlab = '', ylab = '', col = rev(legend_cols),
  ##       xlim = c(1.5, 1.9))
  ## graphics::text(legend_x, seq_along(legend_ats), labels = sprintf('%1.1f', legend_ats),
  ##      cex = 1.5, col = grDevices::grey.colors(1, .2, .2))
  ## graphics::mtext(side = 3, line = 2, text = 'Power')
  ## graphics::box()
  ## ,
  ##         xlim = range(as.numeric(margins_toplot[[2]])))
  ##
  ##
  ## grid lines
  graphics::abline(h = margins_toplot[[1]], v = margins_toplot[[2]], col = 'white')
  ## power contour lines
  power_lwds = ifelse(target_levels == target, 2, 1)
  ## Contour lines
  if (is.na(smooth)){ # no smoothing
    graphics::contour(as.numeric(margins_toplot[[2]]),
            as.numeric(margins_toplot[[1]]),
            t(array_toplot), add = TRUE, labcex = 1.2,
            levels = target_levels, lwd = power_lwds,
            col = grDevices::grey.colors(1, .2, .2))

  } else { # smoothing
    smooth_pred_grid = as.matrix(expand.grid(as.numeric(margins_toplot[[2]]),
                                             as.numeric(margins_toplot[[1]])))
    smooth_z =
      stats::fitted(
               stats::loess(
                        as.vector(ftable(array_toplot, row.vars = 1:2)) ~
                          smooth_pred_grid, span = smooth, degree = 2))
    smooth_z_m =
      stats::xtabs(smooth_z ~ smooth_pred_grid[, 1] + smooth_pred_grid[, 2])
    graphics::contour(as.numeric(margins_toplot[[2]]),
            as.numeric(margins_toplot[[1]]),
            z = smooth_z_m, add = TRUE, labcex = 1.2,
            levels = target_levels, lwd = power_lwds,
            col = grDevices::grey.colors(1, .2, .2))
  }

  graphics::axis(1);graphics::axis(2);graphics::box(bty = 'l')
  ## ============================================
  ## Process Example input
  if (!is.null(example)){
    ## extract (from sse) or calculate (from power_array) example info
    example_list = Example(power_array, example = append(slicer, example),
                           target = target, minimal_target = minimal_target,
                           find_min = find_min, method = method)
    ## Prepare example for figure. Note that it is possible to have any
    ## parameter on x and y, whereas the default is to have 'n' on y.
    y_ex_value = example_list$required_value
    x_ex_name = names(example)
    x_ex_value = example[[x_ex_name]]
  }
  ## note that "y_ex_name" is not defined, this is par_to_search
  ## Draw Example Arrow
  if(!is.null(example)){
    x0 = grDevices::extendrange(graphics::par()$usr[1:2], f = -.02)[1]
    y0 = grDevices::extendrange(graphics::par()$usr[3:4], f = -.02)[1]
    graphics::arrows(x0 = x_ex_value, y0 = y0,
           x1 = x_ex_value, y1 = y_ex_value, length = .15, code = 0)
    graphics::arrows(x0 = x_ex_value, y0 = y_ex_value,
           x1 = x0, y1 = y_ex_value, length = .15)
    graphics::points(rep(x_ex_value, 2), rep(y_ex_value, 2),
                     pch = c(19, 1), cex = c(1, 3))
    graphics::text(x = x0, y = y_ex_value, labels = y_ex_value, adj = c(0, -1))
  }
}

## ======================================================= lower level function
## for plotting example =======================================================
##' @title Add an example to an existing power plot
##' @description Add an example arrow to an existing power plot created by
##'   PlotPower.
##' @param x,slicer,example,target,minimal_target,find_min,method See help for
##'   \code{PLotPower}.
##'
##' @param col Color or arrow drawn.
##' @param ... Further arguments are passed the two calls of function
##'   \code{graphics::arrows} drawing the nicked arrow.
##' @return Nothing
##' @author Gilles Dutilh
AddExample = function(x, slicer = NULL, example, target = .9,
                      minimal_target = TRUE, find_min = TRUE,
                      method = 'step', col = 1, ...)
{
  ## extract (from sse) or calculate (from power_array) example info
  example_list = Example(x, example = append(slicer, example),
                         target = target, minimal_target = minimal_target,
                         find_min = find_min, method = method)
  ## Prepare example for figure. Note that it is possible to have any
  ## parameter on x and y, whereas the default is to have 'n' on y.
  y_ex_value = example_list$required_value
  x_ex_name = names(example)
  x_ex_value = example[[x_ex_name]]

  ## note that "y_ex_name" is not defined, this is par_to_search
  ## Draw Example Arrow
  x0 = grDevices::extendrange(graphics::par()$usr[1:2], f = -.02)[1]
  y0 = grDevices::extendrange(graphics::par()$usr[3:4], f = -.02)[1]
  graphics::arrows(x0 = x_ex_value, y0 = y0,
                   x1 = x_ex_value, y1 = y_ex_value, length = .15,
                   code = 0, col = col, ...)
  graphics::arrows(x0 = x_ex_value, y0 = y_ex_value,
         x1 = x0, y1 = y_ex_value, length = .15, col = col, ...)
  graphics::points(rep(x_ex_value, 2), rep(y_ex_value, 2),
                   pch = c(19, 1), cex = c(1, 3), col = col)
  graphics::text(x = x0, y = y_ex_value, labels = y_ex_value, adj = c(0, -1), col = col)
}
