rm(list=ls())

## =============================================================================
## create some material to work with
sse_pars = list(
  n = seq(from = 10, to = 100, by = 10),
  delta = seq(from = 0.1, to = 1.5, by = 0.2), ## effect size
  sd = seq(.1, .9, .2)) ## Standard deviation
PowFun <- function(n, delta, sd){
  ptt = power.t.test(n = n/2, delta = delta, sd = sd,
                     sig.level = 0.05)
  return(ptt$power)
}
power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)

#' Note that AddExample would trigger an error because no plot called.
PowerPlot(power_array, slicer = list(sd = c(.3)))

## ==================================================================
## Tests on AddExample
## ==================================================================
test_that(
  "Error when example list elements are not equally long",
  {expect_error(
     AddExample(power_array, target_value = .8,
                example = list(delta = c(.7, .9), sd = c(.3)), col = 3)
   )})

test_that(
  "Error when example list contains elements not in array (after slicing)",
  {expect_error(
     AddExample(power_array, target_value = .8,
                example = list(delta = c(.7, .9), bla = c(.3, .5)), col = 3)
   )})






