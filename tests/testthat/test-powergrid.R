rm(list=ls())

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

## =============================================================================
#' Test reproducibility of powergrid

rnd_pars <- list(
  mean = seq(from = 0, to = 1, by = 0.2),
  sd = seq(.5, 1.5, .2)
)

RndFun <- function(mean, sd, n=10) {

  out <- rnorm(mean=mean, sd=sd, n=n) |> mean()
  return(out)
}

set.seed(123)
runif(1)
fixed_seed_list <- .Random.seed
rnd_grid1 <-
  PowerGrid(pars = rnd_pars, fun = RndFun, more_args = list(n=50),
            n_iter=100, summarize = FALSE, parallel = FALSE)

.Random.seed <- fixed_seed_list
rnd_grid2 <-
  PowerGrid(pars = rnd_pars, fun = RndFun, more_args = list(n=50),
            n_iter=100, summarize = FALSE, parallel = FALSE)

test_that("random_seed is correctly stored and accessed",
          {expect_identical(attr(rnd_grid1, which = 'random_seed')[[1]],
                            fixed_seed_list)
          })

#' TODO: This test passes interactively, but fails with automated testing
#' It seems to be due to the way testthat handles Random number generation
# test_that("Output is the same with the same .Random.seed",
#           {expect_identical(rnd_grid1,
#                             rnd_grid2)
#           })

