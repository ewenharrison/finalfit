## ----setup, include = FALSE----------------------------------------------
library(testthat)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
expect_length <- function(object, n) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))

  # 2. Call expect()
  act$n <- length(act$val)
  expect(
    act$n == n,
    sprintf("%s has length %i, not length %i.", act$lab, act$n, n)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

## ------------------------------------------------------------------------
mtcars %>%
  expect_type("list") %>%
  expect_s3_class("data.frame") %>% 
  expect_length(11)

## ------------------------------------------------------------------------
test_that("length computed correctly", {
  expect_success(expect_length(1, 1))
  expect_failure(expect_length(1, 2), "has length 1, not length 2.")
  expect_success(expect_length(1:10, 10))
  expect_success(expect_length(letters[1:5], 5))
})

