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


test_that("Check length of Tp and Rp for model1", {
  expect_length(model1$Tp,length(x1))
  expect_length(model1$Tp,length(x2))
  expect_length(model1$Tp,length(y))
  expect_length(model1$Rp,length(x1))
  expect_length(model1$Rp,length(x2))
  expect_length(model1$Rp,length(y))
})

test_that("Check length of Tp and Rp for model2", {
  expect_length(model2$Tp,length(t0))
  expect_length(model2$Tp,length(td))
  expect_length(model2$Tp,length(tn))
  expect_length(model2$Rp,length(t0))
  expect_length(model2$Rp,length(td))
  expect_length(model2$Rp,length(tn))
})