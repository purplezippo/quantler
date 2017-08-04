library(testthat)
library(quantler)

test_check("quantler")

test_that('maxdrawdown returns a list of two elements',{
  expect_equal(length(maxdrawdown(runif(20))), 2)
})



