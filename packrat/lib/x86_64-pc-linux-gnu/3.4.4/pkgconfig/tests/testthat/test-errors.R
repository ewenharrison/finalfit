
context("Errors")

test_that("Arguments must be named", {

  expect_error(
    set_config("foo" = "bar", "foobar"),
    "Some parameters are not named"
  )
})
