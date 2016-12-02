library(convertemp)
context("Temperature conversions")

test_that("fahr_to_kelvin returns the correct value", {
  expect_equal(fahr_to_kelvin(-459.67), 0)
  expect_equal(fahr_to_kelvin(0), 255.37222)
  expect_equal(fahr_to_kelvin(32), 273.15)
  expect_equal(fahr_to_kelvin(10000), 5810.9278)
})

test_that("kelvin_to_celsius returns the correct value", {
  expect_equal(kelvin_to_celsius(0), -273.15)
  expect_equal(kelvin_to_celsius(273.15), 0)
  expect_equal(kelvin_to_celsius(10000), 9726.85)
})

test_that("celsius_to_fahr returns the correct value", {
  expect_equal(celsius_to_fahr(-273.15), -459.67)
  expect_equal(celsius_to_fahr(0), 32)
  expect_equal(celsius_to_fahr(-17.77777778), 0)
  expect_equal(celsius_to_fahr(10000), 18032)
})

test_that("fahr_to_celsius returns the correct value", {
  expect_equal(fahr_to_celsius(-459.67), -273.15)
  expect_equal(fahr_to_celsius(0), -17.77777778)
  expect_equal(fahr_to_celsius(32), 0)
  expect_equal(fahr_to_celsius(10000), 5537.777777778)
})

test_that("kelvin_to_fahr returns the correct value", {
  expect_equal(kelvin_to_fahr(0), -459.67)
  expect_equal(kelvin_to_fahr(255.37222222), 0)
  expect_equal(kelvin_to_fahr(10000), 17540.33)
})

test_that("celsius_to_kelvin returns the correct value", {
  expect_equal(celsius_to_kelvin(0), 273.15)
  expect_equal(celsius_to_kelvin(-273.15), 0)
  expect_equal(celsius_to_kelvin(10000), 10273.1499)
})

test_that("output doesn't go below absolute zero", {
  expect_warning(fahr_to_kelvin(-500), "This temperature is below absolute zero.")
  expect_warning(celsius_to_kelvin(-300), "This temperature is below absolute zero.")
  expect_warning(kelvin_to_celsius(-1), "This temperature is below absolute zero.")
  expect_warning(fahr_to_celsius(-500), "This temperature is below absolute zero.")
  expect_warning(kelvin_to_fahr(-1), "This temperature is below absolute zero.")
  expect_warning(celsius_to_fahr(-300), "This temperature is below absolute zero.")
})
