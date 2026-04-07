rm(list=ls())
## ===============================================================
## Test that the output of SummarizeIterations are correctly assigned in the
## array.

TestFun <- function(prop_min, threshold){
  prop <-runif(n = 1, min = prop_min, max = 1)
  prop - threshold
}
test_pars = list(
  prop_min = seq(from = 0, to = 1, by = 0.05),
  threshold = seq(from = 0, to = 1, by = 0.05))

n_iter = 100
power_array <- PowerGrid(pars = test_pars, fun = TestFun,
                        n_iter = n_iter, summarize = FALSE)
summarized_power_array <- SummarizeIterations(power_array, summary_function = min)
test_result <-
  summarized_power_array[lower.tri(summarized_power_array, diag = TRUE)]
test_that(
  "Test correct assignment of values to output, and user defined function",
  {expect_true(all(test_result >= 0))}
)

## ===============================================================
## Test warning summarizing summarized object

PowFun <- function(n, delta, sd){
  x1 = rnorm(n = n/2, sd = sd)
  x2 = rnorm(n = n/2, mean = delta, sd = sd)
  t.test(x1, x2)$p.value < 0.05
}
sse_pars = list(
  n = seq(from = 20, to = 60, by = 10),
  delta = seq(from = 0.5, to = 1.5, by = 0.25),
  sd = seq(.5, 1.5, .25))

n_iter = 20
power_array = PowerGrid(pars = sse_pars, fun = PowFun,
                        n_iter = n_iter, summarize = TRUE)
test_that(
  "trying to summarize an object that is already summarized throws a warning",
  {expect_error(
    SummarizeIterations(power_array, summary_function = mean)
  )}
)


## ===============================================================
## Test a comparison for a typical use case.
##
## Based on input this no now just uses the results of power.t.test to generate
## the template. The actual iterations are very basic, and are 0 or 1 based on
## the power. We then check if the SummarizeIterations() accurately calculates the mean.
sse_pars = list(
  n = seq(from = 10, to = 60, by = 20),
  delta = seq(from = 0.5, to = 1.5, by = 0.5),
  sd = seq(.5, 1.5, .5))

ClosedFun <- function(n, delta, sd){
  power.t.test(n = n, delta = delta, sd = sd,)$power
}
closed_power_array <- PowerGrid(pars = sse_pars, fun = ClosedFun,
                                summarize = FALSE)
InterFun <- function(n, delta, sd){
  n <- as.character(n)
  delta <- as.character(delta)
  sd <- as.character(sd)
  p <- as.array(closed_power_array)[n,delta,sd]
  rbinom(n = 1, size = 1, prob = p)
}

## Tested with the following seeds (but seed not set).
# set.seed(c(123, 726)[1])
power_array = PowerGrid(pars = sse_pars, fun = InterFun,
                        n_iter = 1469, summarize = FALSE)
summarized_power_array <- SummarizeIterations(power_array,summary_function = mean)
test_that(
  "SummariseIterations accurately summarises iterations, given a typrical power distribution",
  {expect_equal(summarized_power_array, closed_power_array, ignore_attr=TRUE,
                tolerance = 0.02)}
)
