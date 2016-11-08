library(evrspin)
context("rotor object creation")

test_that("rotor() creates rotor objects", {
    expect_match(class(rotor(model = "sw40")), "rotor")
    expect_match(class(rotor(model = "tla110")), "rotor")
    expect_match(class(rotor("custom", "sw", 5, 10)), "rotor")
    expect_match(class(rotor("custom", "fa", 5, 10, 30, 10)), "rotor")
})

test_that("Argument errors for all rotors", {
    expect_error(rotor(model = "a"), "model does not match")
})

test_that("Argument errors for all custom rotors", {
    expect_error(rotor(model = "custom", type = "a"), "type must be either")
    expect_error(rotor(R.min = "a"), "R.min must be numeric.")
    expect_error(rotor(R.min = -1), "R.min cannot be less")
    expect_error(rotor(R.min = 1, R.max = "a"), "R.max must be numeric")
    expect_error(rotor(R.min = 1, R.max = -1), "R.max must be positive")
    expect_error(rotor(R.min = 1, R.max = 1), "R.max must be greater than R.min")
})

test_that("Argument errors specific to fa rotors", {
    expect_error(rotor("custom", "fa", 1, 2, FA.angle = "a"), "FA.angle must be numeric")
    expect_error(rotor("custom", "fa", 1, 2, FA.angle = 100), "FA.angle must be between")
    expect_error(rotor("custom", "fa", 1, 2, 30, FA.diameter = "a"), "FA.diameter must be numeric")
    expect_error(rotor("custom", "fa", 1, 2, 30, FA.diameter = 0), "FA.diameter must be positive")
})

test_that("sed.L and R.ave calculations are correct", {
    # Swinging Bucket
    expect_equal(rotor("sw40")[["sed.L"]], 92.1)
    expect_equal(round(rotor("sw40")[["R.ave"]], 1), 112.8)

    # Fixed Angle
    expect_equal(round(rotor("type45")[["sed.L"]], 1), 41.6)
    expect_equal(round(rotor("type45")[["R.ave"]] + 0.01, 1), 69.9)
})
