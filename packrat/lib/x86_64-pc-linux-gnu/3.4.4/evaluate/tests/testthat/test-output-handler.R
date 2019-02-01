context("Output handlers")

test_that("text output handler is called with text", {
  text <- NULL
  oh <- new_output_handler(text = function(o) text <<- o)

  evaluate("print('abc')", output_handler = oh)
  expect_equal(text, "[1] \"abc\"\n")
})

test_that("graphic output handler not called with no graphics", {
  graphics <- NULL
  oh <- new_output_handler(graphics = function(o) graphics <<- 1)

  evaluate("print('abc')", output_handler = oh)
  expect_equal(graphics, NULL)
})
