library(frost)

context("buildMdz")

dw <- c(-2,-5,2,6,8)
tempMax <- c(10,20,30,25,29)
tmin <- c(-1,-2,3,5,10)
model <- buildMdz(dw,tempMax,tmin)

test_that("Check output ", {
  expect_is(model,"MdzFrostModel")
  expect_is(model@k,"numeric")
})

test_that("Check errors for null arguments ", {

  expect_error(buildMdz(NULL,tempMax,tmin))
  expect_error(buildMdz(dw,NULL,tmin))
  expect_error(buildMdz(dw,tempMax,NULL))
})


context("predMdz")
#TODO podria recibir mas de un valor o un arreglo en dw ??
pred <- predMdz(dw = -3, tempMax = 15, model)

test_that("Check output ", {
  expect_is(pred,"numeric")
})

test_that("Check errors for null arguments ", {

  expect_error(predMdz(dw = -3, tempMax = NULL, model))
  expect_error(predMdz(dw = -3, tempMax = 15, NULL))
  expect_error(bpredMdz(dw = NULL, tempMax = 15, model))
})
