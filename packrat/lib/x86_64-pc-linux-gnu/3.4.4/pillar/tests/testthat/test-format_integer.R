context("format_numeric")

test_that("integer output will use scientific if necessary", {
  x <- 10000000L + 1:3
  expect_pillar_output(x, width = 6, filename = "integer-06.txt")
  expect_pillar_output(x, width = 7, filename = "integer-07.txt")
  expect_pillar_output(x, width = 8, filename = "integer-08.txt")
  expect_pillar_output(x, width = 9, filename = "integer-09.txt")
})
