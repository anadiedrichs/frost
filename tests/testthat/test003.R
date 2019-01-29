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

#checkLenght

context("Checking vector lenghts")
a <- c(1,3,4,5,6)
b <- c(2,4,5,6)
c <- c(1,1,1,1,1)
test_that("CheckLenght ", {
  expect_error(checkLenght(a,b))
  expect_error(checkLenght(c,b))
  expect_true(checkLenght(a,a))
  expect_true(checkLenght(c,a))
})

#checkNoNA
context("Checking noNAs")
d <- c(1,NA,4,5,6)
b <- c(2,4,5,NA)
test_that("CheckNoNA ", {
  expect_error(checkNoNA(b))
  expect_error(checkNoNA(d))
  expect_true(checkNoNA(a))
  expect_true(checkNoNA(c))
})
