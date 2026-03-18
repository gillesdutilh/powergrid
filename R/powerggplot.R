library(ggplot2)

sse_pars = list(
  n = seq(from = 10, to = 100, by = 10),
  delta = seq(from = 0.1, to = 1.5, by = 0.2), ## effect size
  sd = seq(.1, .9, .2)) ## Standard deviation
PowFun <- function(n, delta, sd){
  ptt = power.t.test(n = n/2, delta = delta, sd = sd,
                     sig.level = 0.05)
  return(ptt$power)
}
x = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
slicer = list(sd=.7)
par_to_search = 'n'

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

    ## =========================================================================
    ## New stuff from here
    sliced_df <- PowerDF(sliced_x)

    ggplot(sliced_df, aes(x= delta, y=n)) +
      geom_contour(aes(z=x))



