library(evrspin)
context("rotor object creation")

test_that("rotor() creates rotor objects", {
    expect_match(class(rotor(model = "sw40")), "rotor")
    expect_match(class(rotor(model = "tla110")), "rotor")
    expect_match(class(rotor("custom", "sw", 5, 10)), "rotor")
    expect_match(class(rotor("custom", "fa", 5, 10, 30, 10)), "rotor")
}) 

test_that("rotor() argument errors", {
    expect_error(rotor(model = "a"), "model does not match")
})