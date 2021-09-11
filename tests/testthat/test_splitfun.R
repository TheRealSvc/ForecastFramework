context("function generator")
library(forecast4you)

test_that("output is a list of functions", {
  expect_equal(class(CreateWalkForwardFuns(48,6)), "function")
})
