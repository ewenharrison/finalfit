context("format_factor")

test_that("output test", {
  expect_pillar_output(xp = factor(c(letters[1:5], NA)), filename = "factor.txt")
  expect_pillar_output(xp = ordered(c(letters[1:5], NA)), filename = "ordered.txt")
  expect_pillar_output(xp = factor("a\nb"), filename = "escaped.txt")
})
