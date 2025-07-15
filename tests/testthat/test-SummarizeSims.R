#' Test that SummarizeSims accurately summarises the proportion
#' TODO: Why does PowerGrid not internally call SummarizeSims to summarize

TestFun <- function(prop_min, threshold){
  prop <-runif(n = 1, min = prop_min, max = 1)
  prop >= threshold
}
test_pars = list(
  prop_min = seq(from = 0, to = 1, by = 0.05),
  threshold = seq(from = 0, to = 1, by = 0.05))

#' TODO: This could use a custom defined function like max or min instead.
n_iter = 100
power_array = PowerGrid(pars = test_pars, fun = TestFun,
                        n_iter = n_iter, summarize = FALSE)
summarized_power_array <- SummarizeSims(power_array, summary_function = mean)
test_result <- summarized_power_array[lower.tri(summarized_power_array, diag = TRUE)]
test_that(
  "Summaries from of a power array are correct",
  {expect_true(all(test_result==1))}
)

