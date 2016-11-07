library(evrspin)
context("rcf and rpm conversions")

test_that("argument errors work", {
    expect_error(rcf2rpm(rcf = "a", rotor = rotor("sw40")), "rcf must be numeric")
    expect_error(rcf2rpm(rcf = 1, rotor = "a"), "rotor is not")
    expect_error(rpm2rcf(rpm = "a", rotor = rotor("sw40")), "rpm must be numeric")
    expect_error(rpm2rcf(rpm = 1, rotor = "a"), "rotor is not")
})

test_that("rcf and rpm calculations are correct", {
    expect_equal(round(rcf2rpm(rcf = 1000, rotor = rotor("sw40")), 0), 2814)
    expect_equal(round(rpm2rcf(rpm = 1000, rotor = rotor("sw40")), 0), 126)
})