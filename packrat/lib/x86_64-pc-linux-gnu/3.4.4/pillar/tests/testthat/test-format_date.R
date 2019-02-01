context("format_date")

test_that("output test", {
  expect_pillar_output(as.Date("2017-07-28"), filename = "date.txt")
})
