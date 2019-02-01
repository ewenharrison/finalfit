context("format_asis")

test_that("output test", {
  expect_pillar_output(xp = I(1:3), filename = "asis-number.txt")
  expect_pillar_output(xp = I(list(1, 1:2, 1:3)), filename = "asis-list.txt")
})
