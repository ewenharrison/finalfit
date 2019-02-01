context("format_numeric")

test_that("same pillar at different widths", {
  v <- 10^c(-9, -6, 3, 9)
  x <- pillar(v)
  expect_equal(get_min_width(x$data), 7)
  expect_equal(get_width(x$data), 7)
  expect_pillar_output(xp = v, width = 4, filename = "numeric-04.txt")
  expect_pillar_output(xp = v, width = 7, filename = "numeric-07.txt")
  expect_pillar_output(xp = v, width = 10, filename = "numeric-10.txt")
  expect_pillar_output(xp = v, width = 15, filename = "numeric-15.txt")
  expect_pillar_output(xp = v, width = 22, filename = "numeric-22.txt")
})
