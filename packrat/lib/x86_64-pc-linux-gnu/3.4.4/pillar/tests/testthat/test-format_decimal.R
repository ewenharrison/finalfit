context("format_decimal")

without_color <- function(code) {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old))

  code
}

format_decimal_bw <- function(x, sigfig = 3, ...) {
  without_color(format_decimal(x, sigfig = sigfig, ...))
}

test_that("compute_rhs_digits() works", {
  x <- c(NA, NaN, Inf, 0, 1, 100, 1e10, 0.001, 1e-14)
  expect_equal(
    compute_rhs_digits(x, 3),
    c(0, 0, 0, 0, 0, 0, 0, 3, 14)
  )
  expect_equal(
    compute_rhs_digits(x + 1e-8, 3),
    c(0, 0, 0, 8, 2, 0, 0, 5, 10)
  )
  expect_equal(
    compute_rhs_digits(x, 7),
    c(0, 0, 0, 0, 0, 0, 0, 3, 14)
  )
  expect_equal(
    compute_rhs_digits(x, 7 + 1e-8),
    c(0, 0, 0, 0, 6, 4, 0, 9, 20)
  )
})

test_that("compute_exp() returns NA if not relevant", {
  x <- c(NA, NaN, Inf, 0, 1, 100, 0.001)
  expect_equal(compute_exp(x), c(NA, NA, NA, NA, 0, 2, -3))
})

test_that("special values appear in LHS", {
  x <- c(NA, NaN, Inf)
  f <- format_decimal_bw(x)

  expect_equal(without_color(format_lhs(f)), format(x))
})

test_that("all-positive values get nothing in neg", {
  f <- format_decimal_bw(c(0, Inf))
  expect_equal(format_neg(f), c("", ""))
})

test_that("negative values get - in neg", {
  f <- format_decimal_bw(c(-Inf, Inf))
  expect_equal(format_neg(f), c("-", " "))
})

test_that("trailing zeros removed if whole decimal fraction", {
  f <- format_decimal_bw(c(1.5, 0.5))
  expect_equal(without_color(format_lhs(f)), c("1", "0"))
  expect_equal(format_rhs(f), c("5", "5"))
})

test_that("trailing zeros pad to sigfigs if data not shown", {
  f <- format_decimal_bw(c(1.5, 0.5) + 1e-6)
  expect_equal(without_color(format_lhs(f)), c("1", "0"))
  expect_equal(format_rhs(f), c("50 ", "500"))
})

test_that("sigfigs split between lhs and rhs", {
  x <- c(1.43, 10.43, 100.43)
  f <- format_decimal_bw(x)

  expect_equal(format_lhs(f), format(trunc(x)))
  expect_equal(format_rhs(f), c("43", "4 ", "  "))
})

test_that("leading 0 added to rhs", {
  f <- format_decimal_bw(1.01)

  expect_equal(format_lhs(f), "1")
  expect_equal(format_rhs(f), "01")
})

test_that("values rounded up as expect", {
  f <- format_decimal_bw(c(18.9, 18.99))

  expect_equal(format_lhs(f), c("18", "19"))
  expect_equal(format_rhs(f), c("9", "0"))
})

test_that("values on LHS not rounded", {
  f <- without_color(format_lhs(format_decimal(123456, 3)))
  expect_equal(f, "123456")
})

test_that("corner cases", {
  expect_equal(format_lhs(format_decimal_bw(numeric())), character())
  expect_equal(format_lhs(format_decimal_bw(numeric(), scientific = TRUE)), character())
})

test_that("output test", {
  expect_pillar_output((10^(-3:4)) * c(-1, 1), filename = "basic.txt")
  expect_pillar_output(1.23456 * 10^(-3:3), filename = "decimal-insignif.txt")
  withr::with_options(
    list(pillar.sigfig = 5),
    expect_pillar_output((10^(c(-2:4, 0L))) * c(-1, 1), filename = "basic-signif-5.txt")
  )
  withr::with_options(
    list(pillar.sigfig = 7),
    expect_pillar_output((10^(0:-5)) * c(-1, 1), width = 20, filename = "basic-signif-7.txt")
  )
  expect_pillar_output((10^(5:-5)) + 1e-7, width = 20, filename = "basic-slightly-nonint.txt")
})
