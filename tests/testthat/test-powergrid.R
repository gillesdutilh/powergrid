rm(list=ls())
## ===============================================================
#' TODO: I think this should be in test-SummarizeIterations no?

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
##
n_iter = 20
power_array = PowerGrid(pars = sse_pars, fun = PowFun,
                        n_iter = n_iter, summarize = TRUE)
test_that(
  "trying to summarize an object that is already summarized throws a warning",
  {expect_error(
    SummarizeIterations(power_array, summary_function = mean)
  )}
)

## =============================================================================
#' Similar to the non-monotonic test in FindTarget()

fwd_closed_pars <-
  list(n = seq(20,60, 10),
       delta = seq(0.5, 1.5, 0.25),
       sd = seq(0.5, 1.5, 0.25))
rev_closed_pars <- lapply(fwd_closed_pars, rev)

ClosedFun <-
  function(n, delta, sd){
    power.t.test(n = n, delta = delta, sd = sd)$power
  }
fwd_power_array <- PowerGrid(pars = fwd_closed_pars, fun = ClosedFun,
                                summarize = FALSE)
rev_power_array <- PowerGrid(pars = rev_closed_pars, fun = ClosedFun,
                             summarize = FALSE)

#' The main thing is that contents are the same. The pars attribute is different
#' I would say it is preference whether it should match the input or the dimnames
#' but it should be intentional. Output with example also tested.
test_that(
  "Reverse parameter specification leads to same array contents (attr not tested)",
  {expect_equal(fwd_power_array, rev_power_array, ignore_attr = TRUE)}
)


## =============================================================================
#' Test PowerGrid() with multiple outputs

#' Arbitary function that only sometimes returns both values.
ClosedFunBadReturn =
  function(n, delta, sd){
    t2side = power.t.test(n = n, delta = delta, sd = sd, alternative = "two")$power
    t1side = power.t.test(n = n, delta = delta, sd = sd, alternative = "one")$power

    out <- c("one-sided" = t1side)
    if(sd%%0.5 == 0) out = c(out, "two.sided" = t2side)

    return(out)
  }

test_that(
  "Error occurs when multiple returns are not equal",
  {
    expect_error(PowerGrid(pars = fwd_closed_pars, fun = ClosedFunBadReturn, n_iter = 3));
    expect_error(PowerGrid(pars = fwd_closed_pars, fun = ClosedFunBadReturn))
  })

ClosedFunTwoReturn =
  function(n, delta, sd){
    t2side = power.t.test(n = n, delta = delta, sd = sd, alternative = "two")$power
    t1side = power.t.test(n = n, delta = delta, sd = sd, alternative = "one")$power
    return(c("one-sided" = t1side, "two.sided" = t2side))
  }

two_return_power_array = PowerGrid(pars = fwd_closed_pars, fun = ClosedFunTwoReturn)

test_that("fun_out names returned correctly",
          {expect_identical(dimnames(two_return_power_array)$fun_out,
                           c("one-sided", "two.sided"))}
)

