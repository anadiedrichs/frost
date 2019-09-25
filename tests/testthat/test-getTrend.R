test_that("getTrend", {
  expect_is(getTrend(Tmin = 22.2,t2 = 33.7,n = 15),"data.frame")
  expect_error(getTrend(Tmin = 22.2,t2 = 33.7,n = a))
  expect_error(getTrend(Tmin = 22.2,t2 = "a",n = 15))
  expect_error(getTrend(Tmin = NA,t2 = 33.7,n = 15))
  expect_error(getTrend(Tmin = 22.2,t2 = 33.7,n = 2))
  expect_error(getTrend(Tmin = 22.2,t2 = 33.7,n = 1))
  expect_error(getTrend(Tmin = 42.2,t2 = 33.7,n = 15))
})
