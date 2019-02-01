context("Test the error handlers for hash functions")

test_that("non-vector inputs are detected", {

  expect_that(md5(list(c("foo","bar"),"baz")), throws_error(regexp = "must be raw or character vector", fixed = TRUE))

})

test_that("non-character inputs are detected", {

  expect_that(md5(12), throws_error(regexp = "must be raw or character vector", fixed = TRUE))

})
