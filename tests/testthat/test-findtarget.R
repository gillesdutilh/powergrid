rm(list=ls())
## ===============================================================
## Tests are very simple, but still sufficient.
## For now more extensive testing is Example()

slice <- setNames(seq(0.09,0.89, 0.1), nm = seq(1,9))

#' Minimum target above 0.5 (default) => 0.59
result1 <- FindTarget(x = slice, target_value = 0.5)
test_that(
  "FindTarget (step) identifies correct value, under defaults",
  {expect_equal(result1, 6)}
)

#' Minimum target below 0.5 => 0.09
result2 <- FindTarget(x = slice, target_value = 0.5,
                      target_at_least = FALSE)
test_that(
  "FindTarget (step) identifies correct value, target is a maximum",
  {expect_equal(result2, 1)}
)

#' Maximal target above 0.5 => 0.89
result3 <- FindTarget(x = slice, target_value=0.5,
                      find_lowest = FALSE)
test_that(
  "FindTarget (step) identifies correct value, minimal parameter value searched",
  {expect_equal(result3, 9)}
)

#' Maximal target below 0.5 => 0.49
result4 <- FindTarget(x = slice, target_value = 0.5,
                      target_at_least = FALSE, find_lowest = FALSE)
test_that(
  "FindTarget (step) identifies correct value, target is a maximum & minimal parameter value searched ",
  {expect_equal(result4, 5)}
)
## ===============================================================

#' The values of target_at_least and find_lowest are ignored if method
#' equals "lm", thus tests are failed.

set.seed(123)
slice <- setNames(seq(0.09,0.89, 0.1), nm = seq(0.1,0.9, 0.1)*10)
slice <- slice + rnorm(n = length(slice), sd = 0.1)

suppressWarnings(
  result1 <- FindTarget(x = slice, target_value = 0.5, method = "lm")
)
test_that(
  "FindTarget (lm) identifies correct value, under defaults",
  {expect_equal(result1, 5, ignore_attr = TRUE)}
)

test_that(
  "FindTarget (lm) throws error if minimal target is FALSE",
  {expect_error(FindTarget(x = slice, target_value = 0.5, method = "lm",
                           target_at_least = FALSE))}
)

test_that(
  "FindTarget (lm) throws error if find_lowest is FALSE",
  {expect_error(FindTarget(x = slice, target_value = 0.5, method = "lm",
                           find_lowest = FALSE))}
)
## ===============================================================

#' Test for the artificial case where names are a string that
#' and only some values can convert to numeric

# badname_slice <- setNames(seq(0.09,0.89, 0.1), nm=LETTERS[1:9])
# names(badname_slice)[6] <- "6"
#
# test_that(
#   "If the vector names are not numeric throw error",
#   {expect_error(FindTarget(badname_slice, target_value=0.5))}
# )


