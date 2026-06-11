## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi=100
)

## ----setup--------------------------------------------------------------------
library(powergrid)

## ----bare functionality-------------------------------------------------------
## I aim to create an array with the product of three numbers

## A function taking the product of three numbers:
Prod = function(x, y, z){
  x * y * z
}
## A grid of numbers I want to calculate the products of. Note that the names of
## the list equal the argument names in Prod()
par = list(x = 1:10,
	       y = 1:3,
           z = c(1, 10))
products = PowerGrid(par, Prod)
print(products)

## Now, I can ask: for each combination of y and z, what is the lowest x where
## the product is at least 20?
FindTarget(products,
           par_to_search = 'x',
           find_lowest = TRUE,
           target_at_least = TRUE,
           target_value = 20)
## Note: when both y and z are 1, there is no x so that the product becomes 20.


## ----basic PowerGrid, fig.width=5.5, fig.height=5.5, fig.align='center'-------
## A function that returns the power as a function of three input parameters
PowFun <- function(n, delta, sd){
  ptt = power.t.test(n = n/2,
                     delta = delta,
                     sd = sd,
                     sig.level = 0.03) # the typical 3% alpha threshold
  return(ptt$power)
}
## A list of values of input parameters to study
pars = list( # a normal list
  n = seq(from = 10, to = 60, by = 5), # sample size
  delta = seq(from = 0.5, to = 1.7, by = 0.1), # effect size
  sd = seq(.5, 1, .1) # variability
)
## Apply PowFun to all crossings of the parameters in pars
power = PowerGrid(pars = pars, fun = PowFun)
summary(power)
PowerPlot(power,
          slicer = list(sd = .7))

## ----basic Example------------------------------------------------------------
Example(power,
        example = list(delta = 1.1, sd = .7),
        target_value = .9) # power = 90%

## ----basic PowerPlot, fig.width=5.5, fig.height=5.5, fig.align='center'-------
PowerPlot(power,
          slicer = list(sd = .7),
          example = list(delta = 1.1),
          target_value = .9
          )

## ----GridPlot, fig.width=5.5, fig.height=5.5, fig.align='center'--------------
GridPlot(power,
         target_value = .9, # you need to choose one target level of power
         example = list(delta = 1, sd = .7)) # defined by two parameters now.

## ----pilot data---------------------------------------------------------------
pilot_scores = c(2.1, 4.3, 2.3, 5.2, 1.9, 8.3, 7, 2.6, 2.4, 3.2, 2.1, 2.8, 3.4)

## ----resample MannU, fig.width=5.5, fig.height=5.5, fig.align='center', warning = FALSE----
sse_pars = list(
  n = seq(10, 100, 20),
  delta = seq(.5, 2, .2)) # only effect size
PowFun = function(n, delta, pilot_data){
  arm_1 = sample(pilot_data, n, replace = TRUE)
  arm_2 = sample(pilot_data, n, replace = TRUE) + delta
  significant = wilcox.test(arm_1, arm_2)$p.value < .03 # the typical 3% alpha threshold
  return(significant) # each call of this function gives significant either TRUE
                      # or FALSE
  }
power = PowerGrid(pars = sse_pars,
                  fun = PowFun,
                  more_args = list(pilot_data = pilot_scores), # pass the pilot
                                                               # data on to the
                                                               # fun argument
                  n_iter = 99) # we need to iterate over simulated experiemtns
                               # to get a power. I would take a higher value
                               # than 99; this is to keep the example quick.
summary(power)
PowerPlot(power)

## ----CI example, fig.width=5.5, fig.height=5.5, fig.align='center'------------
CIFun = function(n, delta, sd){
  x1 = rnorm(n, mean = 0, sd = sd)
  x2 = rnorm(n, mean = delta, sd = sd) 
  abs(diff(t.test(x1, x2)$conf.int)) # return the CI-width
}
pars = list( # a normal list
  n = seq(from = 10, to = 60, by = 5), # sample size
  delta = seq(from = 0.5, to = 1.7, by = 0.1), # effect size
  sd = seq(.5, 1, .1) # variability
)
set.seed(1)
CI_array = PowerGrid(pars, CIFun, n_iter = 20) 
summary(CI_array)
## This object now contains, for each parameter combination, the CI-width
## averaged over 20 iterations.

Example(CI_array,
        example = list(delta = .7, sd = .8),
        target_value = .7,
        target_at_least = FALSE,
        find_lowest = TRUE)
## Show results
PowerPlot(CI_array, slicer = list(delta = .7),
          target_levels = c(.6, .7, .8), # this defines the lines
          title = "CI-width as a funtion of sd and n,\nassuming delta = .7",
          shades_of_grey = FALSE) # Grey scale is optimized for situation where
                                  # the array contains power.
AddExample(CI_array,
           slicer = list(delta = .7),
           example = list(sd = .8),
           target_value = .7,
           target_at_least = FALSE)

## ----maximum SD, fig.width=5.5, fig.height=5.5, fig.align='center'------------
sse_pars = list(
  n = c(30, 40),
  delta = seq(from = 0.4, to = 1.2, by = 0.01), ## effect size
  sd = seq(.3, .9, .01)) ## Standard deviation
PowFun <- function(n, delta, sd){
  ptt = power.t.test(n = n/2, delta = delta, sd = sd,
                     sig.level = 0.05)
  return(ptt$power)
}
power_array = PowerGrid(pars = sse_pars, fun = PowFun, n_iter = NA)
Example(power_array,
        example = list(n = 30, delta = .8),
        find_lowest = FALSE,
        target_value = .9)

## ----maximum SD PowerPlot, fig.width=5.5, fig.height=5.5, fig.align='center'----
PowerPlot(power_array,
          slicer = list(n = 30),
          par_to_search = 'sd',
          example = list(delta = .8),
          find_lowest = FALSE,
          target_value = .9)

