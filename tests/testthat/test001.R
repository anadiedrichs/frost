context("dew point calculation")
library(frost)
rh <- 54
temp <- 25
test_that("Testing dew point calculation with modes A, B, C ", {
  expect_equal(calcDewPoint(rh,temp,mode="A"),14.99222, tolerance = .01 )
  expect_equal(calcDewPoint(rh,temp,mode="B"),24.07111, tolerance = .01 )
  expect_equal(calcDewPoint(rh,temp,mode="C"),15.04884, tolerance = .01 )
})
