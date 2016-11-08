library(evrspin)
context("Vesicle size cutoff calculations")

sw <- rotor("sw40")
fa <- rotor("type60")

test_that("Errors are working", {
  # t
  expect_error(sed_time(d = "a", 1, sw), "d must be numeric")
  expect_error(sed_time(d = -1, 1, sw), "d must be numeric")
  expect_error(sed_time(d = -1:10, 1, sw), "d must be numeric")

  # rcf
  expect_error(sed_time(1, rcf = "a", sw), "rcf must be numeric")
  expect_error(sed_time(1, rcf = -1, sw), "rcf must be numeric")
  expect_error(sed_time(1, rcf = -1:10, sw), "rcf must be numeric")

  # rotor
  expect_error(sed_time(1, 1, "a"), "rotor is not a ")

  # vesicle and solvent densities
  expect_error(sed_time(1, 1, sw, vesicle.density = "a"), "vesicle.density must be numeric.")
  expect_error(sed_time(1, 1, sw, solvent.density = "a"), "solvent.density must be numeric.")
  expect_error(sed_time(1, 1, sw, vesicle.density = 1, solvent.density = 2), "vesicle.density must be greater")

  # viscosity
  expect_error(sed_time(1, 1, sw, viscosity = "a"), "viscosity must be numeric")
  expect_error(sed_time(1, 1, sw, viscosity = -1), "viscosity must be numeric")
  expect_error(sed_time(1, 1, sw, viscosity = -1:10), "viscosity must be numeric")
})

test_that("Output is a numeric vector of correct length", {
  expect_match(class(sed_time(1, 1, sw)), "numeric")
  expect_equal(length(sed_time(1, 1, sw)), 1)

  expect_match(class(sed_time(1:10, 1, sw)), "numeric")
  expect_equal(length(sed_time(1:10, 1, sw)), 10)

  expect_match(class(sed_time(1:10, 1:10, sw)), "numeric")
  expect_equal(length(sed_time(1:10, 1:10, sw)), 10)

  expect_match(class(sed_time(1, 1, fa)), "numeric")
  expect_equal(length(sed_time(1, 1, fa)), 1)

  expect_match(class(sed_time(1:10, 1, fa)), "numeric")
  expect_equal(length(sed_time(1:10, 1, fa)), 10)

  expect_match(class(sed_time(1:10, 1:10, fa)), "numeric")
  expect_equal(length(sed_time(1:10, 1:10, fa)), 10)
})

test_that("Calculations are correct", {
  expect_equal(round(sed_time(150, 10000, sw), 0), 138)
  expect_equal(round(sed_time(150, 10000, fa), 0), 38)
})
