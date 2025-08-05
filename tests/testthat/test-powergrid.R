## ===============================================================
#' TODO: I think this should be in test-SummariseSims no?

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
    SummarizeSims(power_array, summary_function = mean)
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
