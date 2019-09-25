x <- c(10,20,30,25,29)

test_that("RMSE test", {
  expect_is(rmse(1:10),"numeric")
  expect_equal(rmse(1:10),6.2, tolerance = .01 )
  expect_equal(rmse(x),23.94, tolerance = .01 )

})
