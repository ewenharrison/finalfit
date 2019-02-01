context("Test the error handlers for rand_* functions")

test_that("rand_bytes detects non-numeric arguments", {

  expect_that(rand_bytes("turnip"), throws_error(regexp = "numeric", fixed = TRUE))

})
