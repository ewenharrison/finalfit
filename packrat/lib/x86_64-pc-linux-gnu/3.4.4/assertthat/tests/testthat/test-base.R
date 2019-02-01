context("Base assertions")

test_that("any message is useful", {
  expect_equal(validate_that(any(TRUE, FALSE)), TRUE)
  
  x <- c(FALSE, FALSE)
  expect_equal(validate_that(any(x)), "No elements of x are true")
})

test_that("all message is useful", {
  expect_equal(validate_that(all(TRUE, TRUE)), TRUE)
  
  x <- c(FALSE, TRUE)
  expect_match(validate_that(all(x)), "Elements .* of x are not true")
})

test_that("custom message is printed", {
  expect_equal(validate_that(FALSE, msg = "Custom message"), "Custom message")
})
