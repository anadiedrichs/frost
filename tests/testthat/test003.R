library(frost)
context("Checking RH values")

test_that("Check RH values ", {
  expect_error(checkRH(-2))
  expect_error(checkRH(101))
  expect_true(checkRH(35))
  expect_true(checkRH(50.2))
})


context("Checking temp values")

test_that("Check temp values ", {
  expect_error(checkTemp(-22))
  expect_error(checkTemp(101))
  expect_true(checkTemp(35))
  expect_true(checkTemp(20.2))
})
