rm(list=ls())

#' TODO:Some of these are tests for FindTarget() or for Example().

## ===============================================================
sse_pars = list(
  n = seq(from = 10, to = 60, by = 5),
  delta = seq(from = 0.5, to = 1.5, by = 0.1),
  sd = seq(0.5, 1.5, 0.1))

ClosedFun = function(n, delta, sd){
  power.t.test(n = n, delta = delta, sd = sd)$power
}
closed_power_array = PowerGrid(pars = sse_pars, fun = ClosedFun,
                                summarize = FALSE)

SimFun = function(n, delta, sd){
  x1 = rnorm(n, mean=0, sd)
  x2 = rnorm(n, mean = delta, sd)

  tt = t.test(x1, x2)
  wt = wilcox.test(x1, x2)

  out = c("t-test" = tt$p.value < 0.05,
          "wilcox test" = wt$p.value < 0.05)

  return(out)
}
set.seed(123)
sim_power_array = PowerGrid(pars = sse_pars, fun = SimFun,
                            n_iter = 10,
                            summarize = TRUE)


test_that(
  "Error about defaults for method='lm' correctly thrown from Example()",
  {expect_error(Example(closed_power_array,
        example = list(delta = 0.6, sd = 1.0),
        target_value = 0.5, target_at_least = FALSE,
        method = "lm"))})

## =============================================================================

test_that(
  "Warns the user about rounding (lm)",
  {expect_warning(Example(closed_power_array, example = list(n = 35, sd = 1.0),
                          target_value = 0.8, method="lm"))}
)

## Suppress the above warning about rounding
## Suppressing warnings is not ideal , but this is not the goal of this test.
suppressWarnings(
result1 <- Example(closed_power_array,
                   example = list(delta = 0.9, sd = 1.0),
                   target_value = 0.8, method = "lm")
)
comparison1 <- ceiling(power.t.test(delta = 0.9, sd=1.0, power=0.8)$n)
test_that(
  "Gives correct required n for an example calculation under argument defaults (lm)",
  {expect_equal(result1$required_value, comparison1,
                ignore_attr = TRUE)}
)

test_that(
  "Gives correct required value under defaults (step)",
  {expect_equal(Example(closed_power_array,
                        example = list(delta = 0.9, sd = 1.0),
                        target_value = 0.8)$required_value, 25)}
)

## =============================================================================
#' power arrays with non-monotonic pars attributed are handled correctly

set.seed(123)
nm_closed_pars <- lapply(sse_pars, sample)
nm_power_array <- PowerGrid(pars = nm_closed_pars, fun = ClosedFun,
                            summarize = FALSE)


result1 <- Example(closed_power_array, example = list(delta = 0.9, sd = 1), target_value = 0.8)
comparison1 <- Example(nm_power_array, example=list(delta = 0.9, sd = 1), target_value = 0.8)
test_that(
  "non-monotonic par attr in power_array still allows example calculation",
  {expect_equal(closed_power_array, nm_power_array, ignore_attr = TRUE)
    expect_equal(result1$required_value, comparison1$required_value)}
)


## =============================================================================
#' Conversion of the target is maybe a bit extreme.
test_that(
  "Can handle small variations in numeric specification",
  {expect_equal(Example(closed_power_array,
                        example = list(delta = 0.90, sd= 1.00),
                        target_value = "0.8000")$required_value, 25)}
)
## =============================================================================
#' Testing printed output of Example. print_comparison is generated from the
#' same summary function. So more of a test for future issues.

Example_test <-
  Example(closed_power_array,
        example = list(delta = 0.6, sd = 1.0),
        target_value = 0.5)

Example_output <- capture.output(Example_test)

pattern_delta <- "delta\\s?=\\s?0.6"
pattern_sd <- "sd\\s?=\\s?1"
pattern_n <- paste0("n\\s?=\\s?", Example_test$required_value)

test_that(
  "Print correctly outputs the requested example and output details",
  {expect_true(any(grepl(pattern = pattern_delta, Example_output)));
    expect_true(any(grepl(pattern = pattern_sd, Example_output)));
    expect_true(any(grepl(pattern = pattern_n, Example_output)))
    }
)
## =============================================================================
#' Simple test that multiple returns are handled correctly.
#'   - Names are correct (really powergrid)
#'   - error if fun_out not specified
#'   - correct value

test_that(
  "Example handles multiple returns",
  {
    expect_identical(dimnames(sim_power_array)$fun_out,
                     c("t-test", "wilcox test"));
    expect_error(Example(sim_power_array,
                         example = list(delta=1.5, sd = 1.0),
                         target_value = 0.8));
    expect_identical(Example(sim_power_array,
                             example = list(delta=1.5, sd = 1.0,
                                            fun_out = "t-test"),
                             target_value = 0.8)$required_value, 10)
  }
)
