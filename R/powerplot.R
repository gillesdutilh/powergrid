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
##'   ### Maximizing a parameter
##'   One may also search not the minimum, as in the case of sample
##'   size, but the maximum, e.g., the highest sd at which a certain power may
##'   still be achieved. In this case, the \code{par_to_search} is sd, and
##'   \code{find_lowest = FALSE}.
##'   ### When smaller is better
##'   In the standard case of power, higher is better, so you search for a
##'   *minimal* level of power. One may however also aim at, e.g., a *maximal*
##'   width of a confidence interval. For this purpose, set \code{target_at_least}
##'   to \code{FALSE}. See Example for more details about `find_lowest` and
##'   `target_at_least`.
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
##' @param ... Further arguments are passed on to function `image`
##'   internally. Most useful for zooming with xlim and ylim.
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
##' PowFun <- function(n, delta, sd){ # power for a t-test at alpha = .05
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
           col = grDevices::grey.colors(1, .2, .2),
           shades_of_grey = TRUE, # do you want shades of grey on background
           example_text = TRUE, # do you want a text next to the Example arrow
           title = NULL,
           par_labels = NULL,
           smooth = NA,
           ...) # dictionary vector of <varname> = <varlabel>
{
  ## =======================================================
  ## process input
  ## =======================================================
  ##
  if (all(class(x) == 'power_array')) # made using powergrid functions
  {
    if(!attr(x, which = 'summarized')){ # if object contains iterations, first
                                        # summarize
      x = SummarizeIterations(x, summary_function)
      warning(paste0(
        "The object 'x' you supplied to PowerPlot contains individual ",
        "iterations. For sensible plotting, these were automatically ",
        "summarized across iterations using the function given in ",
        "argument `summary_function`."), call. = FALSE)
    }
  } else {
    stop("The object 'x' should be of class 'power_array'. ", call. = FALSE)
  }

  ## =======================================================
  ## take slice that should be plotted
  ## =======================================================
  if(!is.null(slicer)){
    sliced_x = ArraySlicer(x = x, slicer = slicer)
  } else {sliced_x = x}
  ##
  ## if there are multiple function returns saved in power_array, give a warning
  ## and take only the first, by setting slicing accordingly.
  if (attr(sliced_x, 'sim_function_nval') > 1) # still multiple outputs
  {
    ## assume the user want the first
    chosen_fun_out = attr(sliced_x, 'dimnames')$fun_out[1]
    sliced_x = ArraySlicer(sliced_x, slicer = list(fun_out = chosen_fun_out))
      warning(paste0("Argument 'x' contains multiple function outputs at each parameter combination (even after possible slicing with argument 'slicer'). \n*** Function output ",
                     chosen_fun_out,
                     " was automatically chosen to be plotted! ***\nTo explicitly choose a function output, do so using argument 'slicer', including 'fun_out = <output name> in that list."), call. = FALSE)
  }
  ## feedback if the number of dimension are not correct
  left_dims = length(dim(sliced_x))
  if (left_dims == 0){
    left_dims = ifelse(length(sliced_x) > 0,
                       1, 0)
  }
  if(!(left_dims %in% c(2, 1))){
    stop(paste0(
        ifelse(is.null(slicer),
               "Input 'x' should be a 2- or 1-dimensional array, but is a ",
               "Slicing 'x' by 'slicer' did not yield the necessary 2- or 1-dimensional, but a "),
        left_dims, "-dimensional array instead."))
  }
  ##
  dimnms = names(dimnames(sliced_x)) # dimension names to plot
  first_dim = dimnms[1]
  if(par_to_search == 'n' & !(par_to_search %in% dimnms)){
    warning(paste0("Argument `par_to_search` was automatically changed from 'n' (the default) to '",
                   first_dim,
                   "'. If you want to search along another dimension, please set `par_to_search` accordingly."), call. = FALSE)
    par_to_search = first_dim
  }
  dimorder = c(par_to_search, dimnms[dimnms != par_to_search])

  ## =======================================================
  ## About example
  ## =======================================================
  if (!is.null(example) | left_dims == 1){
    ## when either explicitly ordered, or in on-dimentional case.
    draw_example = TRUE
  } else {
    draw_example = FALSE
  }
  ##
  ## =======================================================
  ## Graphical preparation
  ## =======================================================
  ##
  array_toplot = aperm(sliced_x, dimorder) # note that array_toplot is only for
                                           # graphical purposes, not a
                                           # power_array opbject
  margins_toplot = dimnames(array_toplot) # what are the values on the axes
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
  Trans = function(x){# Vectorize for mul
    if(!is.null(par_labels)){
      for(i in seq_along(x)){
        if(x[i] %in% names(par_labels)){x[i] = par_labels[[x[i]]]}
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
  ## if input is a 1-dimensional array, create simple line plot
  if (left_dims == 1){
    plot(as.numeric(names(array_toplot)), array_toplot, type = 'n', axes = FALSE,
         xlab = names(dimnames(array_toplot)), ylab = 'Power')
    graphics::abline(v = as.numeric(names(array_toplot)), col = 'lightgrey')
    graphics::lines(as.numeric(names(array_toplot)), array_toplot, col = col)
    graphics::axis(1, at = as.numeric(names(array_toplot)))
    graphics::axis(2, las = 1)
    graphics::box(bty = 'l')
    graphics::title(paste('Power as a function of',
                          Trans(names(margins_toplot)[[1]])))
    ## simple plot always gets example (otherwise, the argument 'example' would
    ## have to be redefined only for this special case). Example is drawn with
    ## the same code as the normal case
    x_ex_value = FindTarget(array_toplot,
                            target_value = target_value,
                            target_at_least = target_at_least,
                            par_to_search = names(dimnames(array_toplot)),
                            find_lowest = find_lowest,
                            method = method)
    y_ex_value = round(array_toplot[as.character(x_ex_value)], 3)
  } else {
    ## the most typical case:
    ## ============================================
    ## Main plot.
    ## Image contains shades of grey or white, creating higher level plot
    image_x = as.numeric(margins_toplot[[2]])
    image_y = as.numeric(margins_toplot[[1]])
    image_z = t(array_toplot)
    graphics::image(image_x, image_y, image_z,
                    ylab = Trans(names(margins_toplot)[[1]]),
                    xlab = Trans(names(margins_toplot)[[2]]),
                    axes = FALSE, col = image_cols, main = title, las = 1,...)
    ##
    ## grid lines
    graphics::abline(h = margins_toplot[[1]], v = margins_toplot[[2]], col = 'white')
    ## power contour lines
    if (!is.null(target_value)) # if a target_value is given
    {
      if (!(target_value %in% target_levels)){ # but not one of levels, attach.
        target_levels = sort(unique(c(target_levels, target_value)))
      }
      power_lwds = ifelse(target_levels == target_value, 2, 1)
    } else {
      power_lwds = 1
    }
    ## Contour lines
    if (is.na(smooth)){ # no smoothing
      graphics::contour(as.numeric(margins_toplot[[2]]),
                        as.numeric(margins_toplot[[1]]),
                        t(array_toplot), add = TRUE, labcex = 1.2,
                        levels = target_levels, lwd = power_lwds,
                        col = col)

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
  }
  ## Draw Example Arrow
  if (draw_example){
    AddExample(x = sliced_x,
               example = example,
               target_value = target_value,
               find_lowest = find_lowest,
               target_at_least = target_at_least,
               col = col[1],
               example_text = example_text)
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
##' @param
##'   x,target_value,target_at_least,find_lowest,method,example_text,summary_function
##'   See help for \code{PowerPlot}.
##' @param slicer A list, internally passed on to \code{\link{ArraySlicer}} to
##'   cut out a (multidimensional) slice from x. You can achieve the same by
##'   appending "slicing" inside argument `example`. However, to assure that the
##'   result of AddExample is consistent with the figure it draws on top of
##'   (PowerPlot or GridPlot), copy the arguments `x` and `slicer` given to
##'   PowerPlot or GridPlot to AddTarget.
##' @param example A list, defining at which value (list element value) of which
##'   parameter(s) (list element name(s)) the example is drawn for a power of
##'   \code{target_value}. You may supply par vector(s) longer than 1 for
##'   multiple examples. If `example` contains multiple parameters to define the
##'   example, all must contain a vector of the same length. Be aware that the
##'   first element of `example` defines the parameter x-axis, so this function
##'   is not fool proof. See argument `slicer` above. If x has only one
##'   dimention, the example needs not be defined.
##' @param col Color of arrow and text drawn.
##' @param ... Further arguments are passed to the two calls of function
##'   \code{graphics::arrows} drawing the nicked arrow.
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
##' PowFun <- function(n, delta, sd){ # power for a t-test at alpha = .05
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
  ## =======================================================
  ## process input
  ## =======================================================
  ##
  ## further args
  args = list(...)
  ## I grasp lwd here to make text and circle lwd match arrows
  if('lwd' %in% names(args)){lwd = args$lwd}else{lwd = 1}
  ## slice
  sliced_x = ArraySlicer(x = x, slicer = slicer)
  one_dim = FALSE # the default situation where the plot has 2 par-dimenstions

  ## =================================
  ## check argument x (partly the same as in PowerPlot)
  ## =================================
  ##
  ## powerplot object
  if (!all(class(sliced_x) == 'power_array')){ # made using powergrid functions
    stop("The object 'x' should be of class 'power_array'. ")
  }
  ## If there are multiple function returns saved in power_array, give a warning
  ## and take only the first, by setting slicing accordingly.
  if (attr(sliced_x, 'sim_function_nval') > 1) # still multiple outputs
  {
    ## assume the user want the first
    chosen_fun_out = attr(sliced_x, 'dimnames')$fun_out[1]
    sliced_x = ArraySlicer(sliced_x, slicer = list(fun_out = chosen_fun_out))
    warning(paste0("Argument 'x' contains multiple function outputs at each parameter combination (even after possible slicing with argument 'slicer'). \n*** Function output ",
                   chosen_fun_out,
                   " was automatically chosen to be plotted! ***\nTo explicitly choose a function output, do so using argument 'slicer', including 'fun_out = <output name> in that list."))
  }
  ## feedback if the number of dimension are not correct
  left_dims = length(dim(sliced_x))
  if (left_dims == 0){
    left_dims = ifelse(length(sliced_x) > 0,
                       1, 0)
  }
  left_dims = left_dims - (length(example) - 1) # because a longer example will
                                              # slice on the first dim
  if(!(left_dims %in% c(2, 1))){
    stop(paste0("The example ", ifelse(is.null(slicer), "", "(after slicing) "),
                "does not define a one-dimensional vector in x, as it should")
         )
  }
  ## =================================
  ## Check example input
  ## =================================
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
    x_ex_name = names(example)[1] # (also if only one parameret in example)
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
  ## note that "y_ex_name" is not defined, this is par_to_search
  ## Draw Example Arrow
  x0 = grDevices::extendrange(graphics::par()$usr[1:2], f = -.02)[1]
  y0 = grDevices::extendrange(graphics::par()$usr[3:4], f = -.02)[1]
  ## =================================
  ## Draw
  ## =================================
  graphics::arrows(x0 = x_ex_value, y0 = y0,
                   x1 = x_ex_value, y1 = y_ex_value, length = .15,
                   code = 0, col = col, ...)
  graphics::arrows(x0 = x_ex_value, y0 = y_ex_value,
                   x1 = x0, y1 = y_ex_value, length = .15, col = col, ...)
  ## point
  graphics::points(x_ex_value, y_ex_value,
                   pch = 19, cex = 1, col = col, lwd = lwd)
  ## circle
  graphics::points(x_ex_value, y_ex_value,
                   pch = 1, cex = 3, col = col,
                   lwd = 1)
  if (example_text){
    graphics::text(x = x0, y = y_ex_value, labels = y_ex_value,
                   adj = c(0, -1), col = col, lwd = lwd)
  }
  invisible(NULL)
}
