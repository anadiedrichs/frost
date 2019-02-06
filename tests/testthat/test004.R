library(frost)
context("buildFAO")

x1 <- rnorm(100,mean=2,sd=5)
x2 <- rnorm(100,mean=1,sd=3)
y <- rnorm(100,mean=0,sd=2)
model1 <- buildFAO(dw = x2,temp=x1,tmin=y)

t0 <- c(3.2,0.8,0.2,2.6,4.4,5.2,2.7,1.2,4.5,5.6) # temperature 2 hours after sunset
td <- c(-4.2,-8.8,-6.5,-6.2,-6.1,2.6,-0.7,-1.7,-1.2,0.1) # dew point 2 hours after sunset
tn <- c(-3.1,-5,-6.3,-5.4,-4,-2.5,-4.8,-5,-4.4,-3.3)

model2 <- buildFAO(dw = td,temp=t0,tmin=tn)

test_that("Check no NULL values ", {

  expect_is(model1,"FAOFrostModel")
  expect_is(model2,"FAOFrostModel")

  expect_is(model1@r2,"numeric")
  expect_is(model1@a,"numeric")
  expect_is(model1@b,"numeric")
  expect_is(model1@c,"numeric")
  expect_is(model1@Rp,"numeric")
  expect_is(model1@Tp,"numeric")

  expect_is(model2@r2,"numeric")
  expect_is(model2@a,"numeric")
  expect_is(model2@b,"numeric")
  expect_is(model2@c,"numeric")
  expect_is(model2@Rp,"numeric")
  expect_is(model2@Tp,"numeric")

})

test_that("Check vectors ", {

  expect_true(is.vector(model1@Rp))
  expect_true(is.vector(model2@Rp))
  expect_true(is.vector(model1@Tp))
  expect_true(is.vector(model2@Tp))

})
test_that("Check length of Tp and Rp for model1", {
  expect_length(model1@Tp,length(x1))
  expect_length(model1@Tp,length(x2))
  expect_length(model1@Tp,length(y))
  expect_length(model1@Rp,length(x1))
  expect_length(model1@Rp,length(x2))
  expect_length(model1@Rp,length(y))
})

test_that("Check length of Tp and Rp for model2", {
  expect_length(model2@Tp,length(t0))
  expect_length(model2@Tp,length(td))
  expect_length(model2@Tp,length(tn))
  expect_length(model2@Rp,length(t0))
  expect_length(model2@Rp,length(td))
  expect_length(model2@Rp,length(tn))
})

test_that("Check argument errors", {
  expect_error(buildFAO(t0,NULL,td)) # null argument
  expect_error(buildFAO(t0,td,y), "Array arguments must have the same length")
})

context("predFAO using buildFAO")

current_temp <- 10
current_dw <- 2
ptmin <- predFAO(model2,current_temp,current_dw)

test_that("Check output ", {
  expect_is(ptmin,"numeric")
  expect_is(predFAO(model2,current_temp,NULL),"numeric")
})

test_that("Check errors ", {

  expect_error(predFAO(NULL,current_temp,current_dw))
  expect_error(predFAO(model2,NULL,current_dw))
  expect_error(predFAO(new("MdzFrostModel",k = -10, kvector=C(8,-10)),current_temp,current_dw))
})


# test buildFAOTemp
context("buildFAOTemp")

model1 <- buildFAOTemp(temp=x1,tmin=y)
model2 <- buildFAOTemp(temp=t0,tmin=tn)

test_that("Check no NULL values ", {

  expect_is(model1,"FAOFrostModel")
  expect_is(model2,"FAOFrostModel")

  expect_is(model1@r2,"numeric")
  expect_is(model1@a,"numeric")
  expect_is(model1@b,"numeric")
  expect_is(model1@c,"numeric")
  expect_is(model1@Rp,"numeric")
  expect_is(model1@Tp,"numeric")

  expect_is(model2@r2,"numeric")
  expect_is(model2@a,"numeric")
  expect_is(model2@b,"numeric")
  expect_is(model2@c,"numeric")
  expect_is(model2@Rp,"numeric")
  expect_is(model2@Tp,"numeric")

})

test_that("Check vectors ", {

  expect_true(is.vector(model1@Rp))
  expect_true(is.vector(model2@Rp))
  expect_true(is.vector(model1@Tp))
  expect_true(is.vector(model2@Tp))

})
test_that("Check length of Tp and Rp for model1", {
  expect_length(model1@Tp,length(x1))
  expect_length(model1@Tp,length(x2))
  expect_length(model1@Tp,length(y))
  expect_length(model1@Rp,length(x1))
  expect_length(model1@Rp,length(x2))
  expect_length(model1@Rp,length(y))
})

test_that("Check length of Tp and Rp for model2", {
  expect_length(model2@Tp,length(t0))
  expect_length(model2@Tp,length(td))
  expect_length(model2@Tp,length(tn))
  expect_length(model2@Rp,length(t0))
  expect_length(model2@Rp,length(td))
  expect_length(model2@Rp,length(tn))
})


test_that("Check argument errors", {
  expect_error(buildFAOTemp(t0,NULL)) # null argument
  expect_error(buildFAOTemp(t0,y), "Array arguments must have the same length")
})
