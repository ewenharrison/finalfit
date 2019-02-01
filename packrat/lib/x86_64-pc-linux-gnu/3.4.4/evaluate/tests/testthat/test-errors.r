context("Errors")

test_that("all code run, even after error", {
  ev <- evaluate(file("error.r"))
  expect_that(length(ev), equals(4))
})

test_that("code aborts on error if stop_on_error == 1L", {
  ev <- evaluate(file("error.r"), stop_on_error = 1L)
  expect_that(length(ev), equals(2))
})

test_that("code errors if stop_on_error == 2L", {
  expect_error(evaluate(file("error.r"), stop_on_error = 2L), "1")
})

test_that("traceback useful if stop_on_error == 2L", {
  expect_error(evaluate(file("error-complex.r"), stop_on_error = 2L), "Error")

  ## Doesn't work because .Traceback not create when code run
  ## inside try or tryCatch. Can't figure out how to work around.
  ## tryCatch(..., error = function(e) {}) doesn't have enough info
  ## in e, or in the call stack.  options(error = function() {}) doesn't
  ## stop error propagation
  # expect_match(.Traceback[[2]], "h()")
  # expect_match(.Traceback[[3]], "g()")
  # expect_match(.Traceback[[4]], "f()")
})
