library(frost)
context("temperature conversion")

test_that("Testing temperature conversion from Kelvin to Celsius ", {
  expect_equal(convert.temperature(from="K", to="C",350),76.85, tolerance = .01 )
  expect_equal(convert.temperature(from ="K", to="C",273.15),0, tolerance = .01 )
})

test_that("Testing temperature conversion from Celsius to Kelvin", {
  expect_equal(convert.temperature(from = "C",to="K",76.85),350, tolerance = .01 )
  expect_equal(convert.temperature(from = "C",to="K",-5),268.15, tolerance = .01 )
})


test_that("Testing temperature conversion from Celsius to Fahrenheit", {
  expect_equal(convert.temperature(from = "C",to="F",0),32, tolerance = .01 )
  expect_equal(convert.temperature(from = "C",to="F",-5),23, tolerance = .01 )
 expect_equal(convert.temperature(from = "C",to="F",45),113, tolerance = .01 )
})


test_that("Testing temperature conversion from Fahrenheit to Celsius", {
  expect_equal(convert.temperature(from = "F",to="C",32),0, tolerance = .01 )
  expect_equal(convert.temperature(from = "F",to="C",23),-5, tolerance = .01 )
  expect_equal(convert.temperature(from = "F",to="C",113),45, tolerance = .01 )
})


test_that("Testing temperature conversion from Fahrenheit to Kelvin", {
  expect_equal(convert.temperature(from = "F",to="K",32),273.15, tolerance = .01 )
  expect_equal(convert.temperature(from = "F",to="K",23),268.15, tolerance = .01 )
  expect_equal(convert.temperature(from = "F",to="K",113),318.15, tolerance = .01 )
})

test_that("Testing temperature conversion from Kelvin to Fahrenheit", {
  expect_equal(convert.temperature(from = "K",to="F",273.15),32, tolerance = .01 )
  expect_equal(convert.temperature(from = "K",to="F",268.15),23, tolerance = .01 )
  expect_equal(convert.temperature(from = "K",to="F",318.15),113, tolerance = .01 )
})
