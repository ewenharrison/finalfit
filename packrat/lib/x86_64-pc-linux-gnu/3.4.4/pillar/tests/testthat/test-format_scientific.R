context("format_scientific")

format_scientific_bw <- function(x, ...) {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old))

  ret <- pillar_shaft(x, ...)
  # Hack: Pretend decimal format requires 100 characters
  ret$dec <- set_width(ret$dec, 100)
  ret <- set_width(ret, 100)
  ret <- set_min_width(ret, min(get_min_widths(ret)))
  format(ret, width = get_min_width(ret))
}

test_that("negative values displayed correct", {
  f <- format_scientific_bw(-0.123)
  expect_equal(unname(format(f)), "-1.23e-1")
})

test_that("exponents correct in presence of NA", {
  f <- format_scientific_bw(c(NA, 1e-5))
  expect_equal(unname(format(f)), c("NA      ", " 1.00e-5"))
})

test_that("output test", {
  expect_pillar_output(10^c(-9, -6, 3, 9), width = 10, filename = "scientific.txt")
  expect_pillar_output((10^c(3, 9, 15, 22)) * c(-1, 1), width = 10, filename = "scientific-short-neg.txt")
  expect_pillar_output(1.25 * 10^(-309:-319), width = 10, filename = "scientific-tiny.txt")
})
