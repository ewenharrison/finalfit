context("format_logical")

test_that("output test", {
  expect_pillar_output(c(TRUE, FALSE), filename = "logical.txt")
})
