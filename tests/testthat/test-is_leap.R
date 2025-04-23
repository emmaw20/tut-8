library(testthat)
source("R/is_leap.R")

test_that("Regular leap years return TRUE", {
    expect_true(is_leap(1992))
    expect_true(is_leap(2000))
})

test_that("Common years return FALSE", {
    expect_false(is_leap(1900))
    expect_false(is_leap(2021))
})

test_that("Year 0 throws an error", {
    expect_error(is_leap(0), "Year 0 does not exist")
})

test_that("Negative year throws an error", {
    expect_error(is_leap(-2020), "positive")
})

test_that("String input throws an error", {
    expect_error(is_leap("2020"), "numeric")
})

test_that("Non-integer numeric input throws an error", {
    expect_error(is_leap(2020.5), "integer")
})

test_that("Missing input throws an error", {
    expect_error(is_leap(), "provide a single year value")
})

test_that("Vector input throws an error", {
    expect_error(is_leap(c(2000, 2004)), "provide a single year value")
})