##' @title Plot the relation between assumed parameters and
##'   requirements for achieving a target power (or other objective)
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
##'   ## Flexible plotting
##'   The plotting is, however, more flexible.
##'   ### Any variable on the axes
##'   You can flip the axes by setting a different \code{par_to_search} (which
##'   defines the y-axis). The other parameter is automatically chosen to be
##'   drawn on the x-axis.
##'
##'   ### Maximizing a parameter
##'   One may also search not the minimum, as in the case of sample
##'   size, but the maximum, e.g., the highest sd at which a certain power may
##'   still be achieved. In this case, the \code{par_to_search} is sd, and
##'   \code{find_lowest = FALSE}.
##'
##'   ### When smaller is better
##'   In the standard case of power, higher is better, so you search for a
##'   *minimal* level of power. One may however also aim at, e.g., a *maximal*
##'   width of a confidence interval. For this purpose, set \code{target_at_least}
##'   to \code{FALSE}. See Example for more details about `find_lowest` and
##'   `target_at_least`.
##'
##'   ### Graphical parameters
##'   The plot takes the \code{\link{graphical parameters}} from `par`. If graphical
##'   parameters are given as arguments to the function they will be passed to
##'   the calls generating axes, titles and plotting. Grid lines are not modified
##'   by graphical parameters to PowerPlot. `lty` is only passed to the plotted
##'   contours (e.g. to prevent dashed axes). For further customisation of the
##'   axes `xaxt` and `yaxt` can be set to "n", and axes can be added afterwards.
##'
##' @param x An object of class `power_array` (from powergrid).
##' @param slicer If the parameter grid for which `x' was constructed has more
##'   than 2 dimensions, a 2-dimensional slice may be cut out using
##'   \code{slicer}, which is a list whose elements define at which values (the
##'   list element value) of which parameter (the list element name) the slice
##'   should be cut out.
##' @param par_to_search The variable whose minimum (or maximum, when
##'   \code{find_lowest == FALSE}) is searched for achieving the
##'   \code{target_levels}.
##' @param find_lowest Logical, indicating whether the example should be found
##'   that minimizes an assumption (e.g., minimal required n) to achieve the
##'   \code{target_value} or an example that maximizes this assumption (e.g.,
##'   maximally allowed SD).
##' @param target_value The power (or whatever the target is) for which the
##'   example, if requested, is drawn. Also defines which of the power lines is
##'   drawn with a thicker line width, among or in addition to the power lines
##'   defined by target_levels.
##' @param target_at_least Logical. Should the target value be minimally
##'   achieved (e.g., power), or maximially allowed (e.g., estimation
##'   uncertainty).
##' @param example If not NULL, a list of length one, defining at which value
##'   (list element value) of which parameter (list element name) the example is
##'   drawn for a power of \code{target_value}. You may supply a vector longer
##'   than 1 for multiple examples.
##' @param method Method used for finding the required \code{par_to_search}
##'   needed to achieve \code{target_value}. Either \code{step}: walking in
##'   steps along \code{par_to_search} or \code{lm}: Interpolating assuming a
##'   linear relation between \code{par_to_search} and \code{(qnorm(x) + qnorm(1
##'   - 0.05)) ^ 2}. The setting \code{lm} is inspired on the implementation in
##'   the \code{sse} package by Thomas Fabbro.
##' @param target_levels For which levels of power (or whichever variable is
##'   contained in x) lines are drawn.
##' @param col Color for the contour lines. Does not effect eventual example
##'   arrows. Therefore, use AddExample.
##' @param shades_of_grey Logical indicating whether greylevels are painted in
##'   addition to isolines to show power levels.
##' @param example_text When an example is drawn, should the the required par
##'   value be printed alongside the arrow(s)
##' @param title Character string, if not \code{NULL}, replaces default figure
##'   title.
##' @param labcex Numeric value passed to `contour, specifying the size of the
##'   contour labels.
##' @param par_labels Named vector with elements named as the parameters
##'   plotted, with as values the desired labels.
##' @param smooth Numeric, defaults to NA, meaning no smoothing. Non NA value is
##'   used as argument \code{span} for smoothing with \code{stats::loess},
##'   regressing the contour values on the x and y-axis. Suggested value is
##'   .35. Functionality implemented for consistency with \code{sse} package,
##'   but use is discouraged, since regressing the contour values flattens the
##'   contour plot, thereby *biasing* the contour lines.
##' @param summary_function If \code{x} is an object of class \code{power_array}
##'   where attribute \code{summarized} is FALSE (and individual iterations are
##'   stored in dimension \code{iter}, the iterations dimension is aggregated by
##'   \code{summary_fun}. Otherwise ignored.
##' @param ... Further arguments to \code{\link{par}}, \code{\link{axis}} and
##'   \code{\link{image}}. A few exceptions (e.g. `y`) are ignored with a warning.
##'   `...` is also passed directly to \code{\link{AddExample}}
##' @seealso \code{\link{PowerGrid}}, \code{\link{AddExample}},
##'   \code{\link{Example}}, \code{\link{GridPlot}} for plotting
##'   interdependencies of 3 parameters.
##' @return A list containing the coordinate arguments x, y, and z, as passed to
##'   `image()` internally.
##' @author Gilles Dutilh
##' @examples
##' ## ============================================
##' ## Typical use case: minimal n for power
##' ## ============================================
##' ## What's the minimal sample size n, given the combination of sd and delta.
##'
##' ## Set up a grid of n, delta and sd:
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 4),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.1), # effect size
##'   sd = seq(.1, 1.1, .2)) # Standard deviation
##'
##' ## Define a power function using these parameters:
##' PowFun = function(n, delta, sd){ # power for a t-test at alpha = .05
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = 0.05)
##'   return(ptt$power)
##' }
##'
##' ## Evaluate PowFun across the grid defined by sse_pars:
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##'
##' ## explore power graphically in the situation where sd = .7, including an
##' ## example situation where delta is .9:
##' PowerPlot(power_array,
##'           slicer = list(sd = .7),
##'           example = list(delta = c(.7, .9)), # two examples
##'           target_value = .9 # 90% power
##'           )
##'
##' ## Some graphical adjustments. Note that example is drawn on top of
##' ## PowerPlot now.
##' PowerPlot(power_array,
##'           slicer = list(sd = .7),
##'           par_labels = c(n = 'Total Sample Size',
##'                          delta = 'Effect Size',
##'                          sd = 'Standard Deviation'),
##'           target_levels = c(.8, .9), # draw fewer power isolines
##'           target_value = NA # no specific power target (no line thicker)
##'           )
##' AddExample(power_array,
##'            slicer = list(sd = .7),
##'            example = list(delta = .9),
##'            target_value = .9,
##'            col = 'Orange', lwd = 3)
##'
##' ## ============================================
##' ## Less typical use case:
##' ## minimal delta for power, given sd, as a function of n
##' ## ============================================
##' ## You can easily change what you search for. For example: At each sample size n,
##' ## what would be the minimal effect size delta there must be for the target
##' ## power to be achieved?
##'
##' PowerPlot(power_array,
##'           par_to_search = 'delta',
##'           slicer = list(sd = .7))
##'
##' ## ============================================
##' ## Less typical use case:
##' ## *maximum sd* for power, given n, as a function of delta
##' ## ============================================
##' ## You're not limited to study n at all, nor to searching a minimum: When
##' ## your n is given to be 30, what is the largest sd at which we still find
##' ## enough power? (as a function of delta on the x-axis)
##'
##' PowerPlot(power_array,
##'           par_to_search = 'sd',
##'           find_lowest = FALSE,
##'           slicer = list(n = 30))
##'
##' ## Adding an example works the same: If we expect a delta of 1, and the n =
##' ## 30, what is the maximal SD we can have still yielding 90% power?
##'
##' AddExample(power_array,
##'            find_lowest = FALSE,
##'            slicer = list(n = 30),
##'            example = list(delta = 1),
##'            target_value = .9)
##' @export
PowerPlot =
  function(x, # object of class `power_array` or powEx output (class `power`)
           slicer = NULL, # which plain of the grid
           par_to_search = 'n', # default, for what should we find the min/max
           example = NULL, # a list(<parameter> = <value>)
           find_lowest = TRUE, # search for min or max in par_to_search
           target_value = .9, # the minimum (or maximum, see below)
           target_at_least = TRUE,
           method = 'step',
           summary_function = mean,
           target_levels = c(.8, .9, .95), # which power iso lines to draw
           col = grDevices::grey.colors(1, .2, .2), # TODO: Decide whether to fix this.
           shades_of_grey = TRUE, # do you want shades of grey on background
           example_text = TRUE, # do you want a text next to the Example arrow
           title = NULL,
           labcex = 1.2, # cex specifically for the labels on the contours
           par_labels = NULL,
           smooth = NA,
           ...) # Ellipsis passed to various internal calls
  {

    ## =======================================================
    ## process power array
    ## =======================================================
    if (!inherits(x, "power_array")) stop("The object 'x' should be of class 'power_array'. ", call. = FALSE)

    x = EnsureSummarized(x, summary_function = summary_function)

    ## =======================================================
    ## take slice that should be plotted
    ## =======================================================
    ## TODO: this if structure is not needed as ArraySlicer seems to
    ## return a unchanged array if slicer = NULL, incidental behaviour though.
    if(!is.null(slicer)){
      sliced_x = ArraySlicer(x = x, slicer = slicer)
    } else {sliced_x = x}

    sliced_x = EnsureSingleFunOut(sliced_x)

    left_dims = CheckArrayDim(sliced_x, required_dim = c(1,2))

    ## =======================================================
    ## Get the name of the parameter to search (typically n)
    ## =======================================================
    dimnms = names(dimnames(sliced_x))
    first_dim = dimnms[1]
    if(par_to_search == 'n' & !(par_to_search %in% dimnms)){
      warning(paste0(
        "Argument `par_to_search` was automatically changed from 'n' (the default) to '",
        first_dim,
        "'. If you want to search along another dimension, please set `par_to_search` accordingly."
      ), call. = FALSE)
      par_to_search = first_dim
    }
    dimorder = c(par_to_search, dimnms[dimnms != par_to_search])

    ## =======================================================
    ## Graphical preparation
    ## =======================================================
    array_toplot = aperm(sliced_x, dimorder)
    margins_toplot = dimnames(array_toplot)

    if(shades_of_grey){
      n_breaks = 101
      breaks = seq(0, 1, length = n_breaks)
      image_cols = grDevices::grey.colors(length(breaks) - 1, start = 0.3, end = .9)
      legend_ats = pretty(array_toplot)
      legend_cols = image_cols[
        cut(legend_ats, breaks, labels = 1:(n_breaks-1), include.lowest = TRUE)]
    } else {
      image_cols = grDevices::grey.colors(1, .9, .9)
    }

    Trans = function(x){
      if(!is.null(par_labels)){
        for(i in seq_along(x)){
          if(x[i] %in% names(par_labels)){x[i] = par_labels[[x[i]]]}
        }
      }
      return(x)
    }

    slice_at = slicer[lapply(slicer, length) == 1]

    ## =======================================================
    ## User graphical arguments
    ## =======================================================
    ## Allow flexible parameter specification via ellipsis
    dots = list(...)

    good_args = c(names(graphics::par()),
                  names(formals(graphics::axis)),
                  names(formals(graphics::lines)),
                  names(formals(graphics:::plot.default)),
                  names(formals(graphics:::image.default)))
    good_args = setdiff(good_args, "...")

    bad_args = setdiff(names(dots), good_args)
    if (length(bad_args) > 0) {
      warning("Only arguments to par(), axis(), lines(), plot.default() and image.defualt() can be supplied through `...` the following are ignored: ",
              paste(bad_args, collapse = ", "), call. = FALSE)
      dots[bad_args] = NULL
    }

    ## do not allow dots to override core internals
    exeption_bad_args = intersect(names(dots), c("y", "z", "type", "at"))
    if (length(exeption_bad_args) > 0) {
      warning("These arguments cannot be supplied through `...` and are ignored: ",
              paste(exeption_bad_args, collapse = ", "), call. = FALSE)
      dots[exeption_bad_args] = NULL
    }

    ## Get the plot title priority is title arg > main arg > internal
    if ("main" %in% names(dots)) {
      if (is.null(title)) title = dots$main
      dots$main = NULL
    }
    if (is.null(title)){
      title = ifelse(
        is.null(slicer) | all(names(dimnames(array_toplot)) == names(slicer)),
        "Power",
        paste('Power at',
              paste(Trans(names(slice_at)), '=', slice_at, collapse = ', '))
      )
    }

    ## TODO: I am pretty sure these are no longer needed.
    # ## Get bty and las from dots if specified, otherwise use par values
    # dots$las = if ("las" %in% names(dots)) dots$las else graphics::par()$las
    # dots$bty = if ("bty" %in% names(dots)) dots$bty else graphics::par()$bty
    #
    # ## If lwd is specified use that, otherwise take lwd from graphics::par().
    # ## Later it has to be omitted from the dots passed to the contour
    # dots$lwd = if ("lwd" %in% names(dots)) dots$lwd else graphics::par()$lwd

    ## Only let lty affect certain plot characteristics, so remove from dots
    user_lty = if ("lty" %in% names(dots)) dots$lty else NULL
    dots$lty = NULL

    ## Make lists of all the dots arguments to be passed to each function.
    par_dots <- dots[intersect(names(dots),names(graphics::par()))]
    image_dots <- dots[intersect(names(dots), c(names(graphics::par()), names(formals(graphics:::image.default))))]
    plot_dots <- dots[intersect(names(dots), c(names(graphics::par()), names(formals(graphics:::plot.default))))]
    lines_dots <- dots[intersect(names(dots), c(names(graphics::par()), names(formals(graphics:::lines))))]
    axis_dots <- dots[intersect(names(dots), c(names(graphics::par()), names(formals(graphics::axis))))]

    ## Contour is awkward, so it just gets graphics::par() args. lwd is not specified
    ## so it can get varying values.
    contour_dots <- par_dots[!names(par_dots) %in% c("lwd")]

    ## Add back the lty to certain dots
    lines_dots$lty = contour_dots$lty = user_lty

    ## =======================================================
    ## Draw 1d figure
    ## =======================================================
    if (left_dims == 1){

      x_vals = as.numeric(names(array_toplot))

      if(is.null(plot_dots$xlab)) plot_dots$xlab = Trans(names(dimnames(array_toplot)))
      if(is.null(plot_dots$ylab)) plot_dots$ylab = "Power"
      plot_main = if (!is.null(title)) {
        title
      } else {
        paste('Power as a function of', Trans(names(margins_toplot)[[1]]))
      }

      ## make empty plot, add vertical gridlines
      do.call(graphics::plot, append(list(x = x_vals,
                                          y = array_toplot,
                                          type = 'n',
                                          axes = FALSE),
                                     plot_dots))
      graphics::abline(v = x_vals, col = 'lightgrey')

      ## Add contour
      do.call(graphics::lines, append(list(x= x_vals,
                                           y= array_toplot,
                                           col = col),
                                      lines_dots))

      ## Add axes
      do.call(graphics::axis, append(list(side=1,
                                          at = x_vals),
                                     axis_dots))
      do.call(graphics::axis, append(list(side=2), axis_dots))

      ## Border and title
      do.call(graphics::box, par_dots)
      do.call(graphics::title, append(list(main = plot_main), par_dots))

      ## Add an example
      ## TODO: y_ex_value does not seem to be used.
      x_ex_value = FindTarget(array_toplot,
                              target_value = target_value,
                              target_at_least = target_at_least,
                              par_to_search = names(dimnames(array_toplot)),
                              find_lowest = find_lowest,
                              method = method)
      y_ex_value = round(array_toplot[as.character(x_ex_value)], 3)

      image_x = image_y = image_z = NULL

    } else
      ## =======================================================
    ## Draw 2d figure
    ## =======================================================
    {
      image_x = as.numeric(margins_toplot[[2]])
      image_y = as.numeric(margins_toplot[[1]])
      image_z = t(array_toplot)

      if(is.null(image_dots$xlab)) image_dots$xlab = Trans(names(margins_toplot)[[2]])
      if(is.null(dots$ylab)) image_dots$ylab = Trans(names(margins_toplot)[[1]])

      image_args = c(
        list(x = image_x,
             y = image_y,
             z = image_z,
             axes = FALSE,
             col = image_cols,
             main = title),
        image_dots
      )
      do.call(graphics::image, image_args)

      ## Draw gridlines (don't receive dots)
      graphics::abline(h = margins_toplot[[1]], v = margins_toplot[[2]],
                       col = 'white')

      ## If a target power is specified double the lwd on the target.
      if (!is.null(target_value)) {
        if (!(target_value %in% target_levels)) {
          target_levels = sort(unique(c(target_levels, target_value)))
        }
        power_lwds = ifelse(target_levels == target_value, 2, 1) * dots$lwd
      } else {
        power_lwds = dots$lwd
      }

      ## Contour is a bit funny in it arguments.
      contour_args =
        append(list(x = as.numeric(margins_toplot[[2]]),
                    y = as.numeric(margins_toplot[[1]]),
                    z = t(array_toplot), add = TRUE, labcex = labcex,
                    levels = target_levels, lwd = power_lwds,
                    col = col),
               contour_dots)

      ## For smoothed plotting only the z calculation differs now
      if (!is.na(smooth)) {
        smooth_pred_grid = as.matrix(expand.grid(as.numeric(margins_toplot[[2]]),
                                                 as.numeric(margins_toplot[[1]])))
        smooth_z =
          stats::fitted(
            stats::loess(
              as.vector(ftable(array_toplot, row.vars = 1:2)) ~ smooth_pred_grid,
              span = smooth, degree = 2))
        smooth_z_m =
          stats::xtabs(smooth_z ~ smooth_pred_grid[, 1] + smooth_pred_grid[, 2])
        contour_args$z = smooth_z_m
      }
      do.call(graphics::contour, contour_args)
      do.call(graphics::axis, append(list(side=1), axis_dots))
      do.call(graphics::axis, append(list(side=2), axis_dots))
      do.call(graphics::box, args = par_dots)
    }

    ## =======================================================
    ## About example
    ## =======================================================
    draw_example = !is.null(target_value) &&
      (!is.null(example) | left_dims == 1)
    if (draw_example){

      target_value_logical = target_levels %in% target_value
      if(length(target_value_logical) == length(col)) {
        COL = col[target_value_logical]
      } else COL = col[1]

      AddExample(x = sliced_x,
                 example = example,
                 target_value = target_value,
                 find_lowest = find_lowest,
                 target_at_least = target_at_least,
                 col = COL,
                 example_text = example_text,
                 ...)
    }

    invisible(list('image_args' = list('x' = image_x, 'y' = image_y, 'z' = image_z)))
  }

## ======================================================= lower level function
## for plotting example =======================================================
##' @title Add an example to an existing PowerPlot or GridPlot
##' @description
##'
##' Add example arrow(s) to an existing figure created by PowerPlot
##' or GridPlot.
##'
##' `AddExample` is a higher level plotting function, so it does not know
##' anything about the figure it draws on top off. Therefore, take care your
##' figure makes sense, by supplying the same arguments \code{x} and
##' \code{slicer} that you supplied to the \code{\link{PowerPlot}} or
##' \code{link{GridPlot}} you are drawing on top off: With \code{slicer} you
##' define the plotted plain, with \code{example} the value on the x-axis where
##' the arrow starts. To be sure of a sensible result, use the argument
##' `example` inside \code{Powerplot} or \code{GridPlot}.

##' @details
##' ## arguments \code{slicer} and \code{example}
##'
##' `slicer` takes the slice of x that is in the figure, `example` defines at
##' which value of which parameter, the example is drawn. These arguments' use
##' is the same as in PowerPlot and GridPlot. If you want to make sure that the
##' result of AddExample is consistent with a figure previously created using
##' PowerPlot or GridPlot, copy the argument `slicer` to such function to
##' AddExample, and define your example in `example`.
##'
##' Note however, that:
##'
##' slicer = list(a = c(1, 2)) and example = list(b = c(3, 4))
##'
##' has the same result as:
##'
##' example = list(b = c(3, 4) and a = c(1, 2))  (not defining slicer)
##'
##' Importantly, the the order of `example` matters here, where the first
##' element defines the x-axis.
##'
##' ## multiple examples
##'
##' Argument \code{example} may contain vectors with length longer than one to
##' draw multiple examples.
##'
##' @param x Either a power array, or a power_example produced by \code{\link{Example}}.
##' @param slicer A list, internally passed on to \code{\link{ArraySlicer}} to
##'   cut out a (multidimensional) slice from x. You can achieve the same by
##'   appending "slicing" inside argument `example`. However, to assure that the
##'   result of AddExample is consistent with the figure it draws on top of
##'   (PowerPlot or GridPlot), copy the arguments `x` and `slicer` given to
##'   PowerPlot or GridPlot to AddTarget.
##' @param target_value,target_at_least,find_lowest,method,example_text,summary_function
##'   See help for \code{PowerPlot}. Ignore if x is a power_example.
##' @param example A list, defining at which value (list element value) of which
##'   parameter(s) (list element name(s)) the example is drawn for a power of
##'   \code{target_value}. You may supply par vector(s) longer than 1 for
##'   multiple examples. If `example` contains multiple parameters to define the
##'   example, all must contain a vector of the same length. Be aware that the
##'   first element of `example` defines the parameter x-axis, so this function
##'   is not fool proof. See argument `slicer` above. If x has only one
##'   dimension, the example needs not be defined. Ignored if x is a power_example.
##' @param col Color of arrow and text drawn.
##' @param ... Further arguments to \code{\link{par}}, as well as `length` and `angle`
##' for arrows. These are passed to the points, arrows and text. For the points
##' `pch` is fixed.
##' @seealso \code{\link{PowerPlot}}, \code{\link{GridPlot}}
##' @return invisibly NULL
##' @author Gilles Dutilh
##' @examples
##'
##' ## For more examples, see ?PowerPlot
##'
##' ## Set up a grid of n, delta and sd:
##' sse_pars = list(
##'   n = seq(from = 10, to = 60, by = 4),
##'   delta = seq(from = 0.5, to = 1.5, by = 0.1), # effect size
##'   sd = seq(.1, 1.1, .2)) # Standard deviation
##' ## Define a power function using these parameters:
##' PowFun = function(n, delta, sd){ # power for a t-test at alpha = .05
##'   ptt = power.t.test(n = n/2, delta = delta, sd = sd,
##'                      sig.level = 0.05)
##'   return(ptt$power)
##' }
##' ## Evaluate PowFun across the grid defined by sse_pars:
##' power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
##'
##' ## ======================
##' ## PowerPlot
##' ## ======================
##' PowerPlot(power_array,
##'           slicer = list(sd = .7),
##'           )
##' AddExample(power_array,
##'            slicer = list(sd = .7), # be sure to cut out the same plain as above
##'            example = list(delta = .9),
##'            target_value = .9,
##'            col = 'blue')
##' AddExample(power_array,
##'            slicer = list(sd = .7),
##'            example = list(delta = c(.7, 1)), # multiple examples
##'            target_value = .9,
##'            col = 'yellow')
##' ## Careful, you can move the slicer argument to example:
##' AddExample(power_array,
##'            example = list(delta = 1.2, sd = .7), # delta (x-axis) first
##'            target_value = .9,
##'            col = 'green')
##' ## Careful, because you can put the wrong value on x-axis!
##' AddExample(power_array,
##'            example = list(sd = .7, delta = 1.2), # sd first?!
##'            target_value = .9,
##'            col = 'red')
##'
##' ## ======================
##' ## GridPlot
##' ## ======================
##' GridPlot(power_array, target_value = .9)
##' AddExample(power_array,
##'            example = list(delta = 1, sd = .7),
##'            target_value = .9
##'            )
##' ## two examples
##' AddExample(power_array,
##'            example = list(delta = c(.9, 1.2), sd = c(.5, 1.1)),
##'            target_value = .9, col = 3
##'            )
##' @export
AddExample = function(x,
                      slicer = NULL,
                      example = NULL,
                      find_lowest = TRUE,
                      target_value = NULL,
                      target_at_least = TRUE,
                      method = 'step',
                      summary_function = mean,
                      col = grDevices::grey.colors(1, .2, .2),
                      example_text = TRUE, ...)
{
  ## Initially assume the default situation where the plot has 2 par-dimensions
  one_dim = FALSE

  ## =======================================================
  ## Check type of input
  ## =======================================================
  if(inherits(x, "power_array")) {

    x = EnsureSummarized(x, summary_function = summary_function)

    sliced_x = ArraySlicer(x = x, slicer = slicer)

    sliced_x = EnsureSingleFunOut(sliced_x)

    ## This is translated from Gilles, I don't quite get the -1 for the example
    left_dims = CheckArrayDim(sliced_x, required_dim = c(1,2) + (length(example) - 1)
    )

    ## =======================================================
    ## Check example input
    ## =======================================================
    ## User may define multiple requested examples. Are they correctly defined and
    ## How many are there?
    if(!is.null(example)){
      if(length(unique(sapply(example, function(x)length(x)))) != 1){
        ## if more than one example is requested, each parameter in example must have
        ## the same length
        stop("If multiple pars are listed in argument 'example', all must contain a vector of the same length.")
      }
      ns_example = sapply(example, function(x)length(x))[[1]]
      ## The first element of example always defines the par on the x-axis
      x_ex_name = names(example)[1] # (also if only one parameter in example)
    } else { # if example NULL
      if (length(dim(sliced_x)) > 1){
        stop("When x (after slicer has been applied) has more than one dimension, 'example' must be supplied")
      }
      one_dim = TRUE # we're in the on-dimensional situation, where we plot the
      # value (power) on the y-axis
      ns_example = 1
    }

    ## =================================
    ## Prepare example(s) for plotting
    ## =================================
    ## essentially loop over each individual example
    ## TODO (Future): This would be cleaner to do with FindTarget
    y_ex_value = numeric(ns_example)
    x_ex_value = numeric(ns_example)
    for (example_i in 1:ns_example){
      ## run over examples and calculate and store coordinates
      cur_example = lapply(example, function(x)x[example_i])
      example_list =
        Example(sliced_x,
                example = cur_example, # append(slicer, cur_example),
                target_value = target_value, target_at_least = target_at_least,
                find_lowest = find_lowest, method = method, summary_function = summary_function)
      ## Prepare example for figure. Note that it is possible to have any
      ## parameter on x and y, whereas the default is to have 'n' on y.
      if (!one_dim){
        y_ex_value[example_i] = example_list$required_value
        x_ex_value[example_i] = cur_example[[x_ex_name]]
      } else {
        x_ex_value[example_i] = example_list$required_value
        y_ex_value[example_i] = example_list$target_value
      }
    }

  } else if(inherits(x, "power_example")) {
    ## TODO (Future): either warn of specified power etc or check against example

    input_example = x

    if(is.null(slicer)) one_dim = TRUE


    if (!one_dim){

      left_par <- setdiff(names(input_example$requested_example), names(slicer))

      if(length(left_par) != 1) stop("slicer argument incompatable with provided x")

      x_ex_value <- input_example$requested_example[[left_par]]
      y_ex_value <- input_example$required_value

    } else {
      x_ex_value = input_example$required_value
      y_ex_value = input_example$target_value
    }
  } else {
    stop("x must either be a power_array or a power_example")

  }

  ## =======================================================
  ## Get graphics parameters from the dots
  ## =======================================================
  ## To make life simple, I only allow formals from graphics::par(), this ensures
  ## the dots from GridPlot or PowerPlot will also be valid
  ## after filtering.

  ## If called within GridPlot or PowerPlot these come directly from the function
  ## call (so they need to be filtered again.
  dots = list(...)

  ## Run directly from the global environment. If run from GridPlot or PowerPlot
  ## repeat warnings add confusion.
  top_level <- identical(parent.frame(), .GlobalEnv)

  good_args = union(names(graphics::par()), c("length", "angle"))
  bad_args = setdiff(names(dots), good_args)
  if (length(bad_args) > 0) {
    if(top_level) {
      warning("Only arguments to par(), as well as length and angle can be supplied through `...` the following are ignored: ",
            paste(bad_args, collapse = ", "), call. = FALSE)
    }
    dots[bad_args] = NULL
  }

  if(!"cex" %in% names(dots)) dots$cex = graphics::par()$cex

  ## Arrows take all arguments
  arrows_dots = dots
  ## Text only gets par
  text_dots = dots[intersect(names(dots), names(graphics::par()))]
  ## points just gets par(), cex is omitted so we can specify the inner to outer ratio
  points_dots = dots[intersect(names(dots), setdiff(names(graphics::par()), c("cex", "pch")))]


  ## =================================
  ## Draw
  ## =================================

  ## note that "y_ex_name" is not defined, this is par_to_search
  ## Draw Example Arrow
  x0 = grDevices::extendrange(graphics::par()$usr[1:2], f = -.02)[1]
  y0 = grDevices::extendrange(graphics::par()$usr[3:4], f = -.02)[1]

  ## Horizontal arrow (code = 2 for arrow at the end)
  do.call(graphics::arrows, append(list(x0 = x_ex_value,
                                        y0 = y_ex_value,
                                        x1 = x0,
                                        y1 = y_ex_value,
                                        code = 2,
                                        col = col),
                                   arrows_dots))

  ##Vertical arrow (no arrowhead)
  do.call(graphics::arrows, append(list(x0 = x_ex_value,
                                        y0 = y0,
                                        x1 = x_ex_value,
                                        y1 = y_ex_value,
                                        code = 0,
                                        col = col),
                                   arrows_dots))

  ## point
  do.call(graphics::points, append(list(x = x_ex_value,
                                        y = y_ex_value,
                                        pch = 19,
                                        cex = dots$cex *1,
                                        col = col),
                                   points_dots))


  ## circle
  do.call(graphics::points, append(list(x = x_ex_value,
                                        y = y_ex_value,
                                        pch = 1,
                                        cex = dots$cex *3,
                                        col = col),
                                   points_dots))

  if (example_text){
    do.call(graphics::text, append(list(x = x0,
                                        y = y_ex_value,
                                        labels = y_ex_value,
                                        adj = c(0, -1),
                                        col = col),
                                   text_dots))
  }
  invisible(NULL)
}

