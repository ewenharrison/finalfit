library(testthat)
context("Test txtMergeLines")

test_that("Check one argument with multiple new lines",{
  out <- txtMergeLines("a
                       b")
  expect_equal(length(gregexpr("<br>", out)[[1]]),
               1)

  out <- txtMergeLines("a
                       b
                       c")
  expect_equal(length(gregexpr("<br>", out)[[1]]),
               2)
})

test_that("Check multiple arguments",{
  out <- txtMergeLines("a", "b")
  expect_equal(length(gregexpr("<br>", out)[[1]]),
               1)

  out <- txtMergeLines("a", "b", "c")
  expect_equal(length(gregexpr("<br>", out)[[1]]),
               2)
})