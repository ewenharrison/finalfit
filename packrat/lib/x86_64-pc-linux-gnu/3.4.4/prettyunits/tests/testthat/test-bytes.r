
context("Pretty bytes")

test_that("pretty_bytes gives errors on invalid input", {

  expect_error(pretty_bytes(''), 'is.numeric.*is not TRUE')
  expect_error(pretty_bytes('1'), 'is.numeric.*is not TRUE')
  expect_error(pretty_bytes(TRUE), 'is.numeric.*is not TRUE')
  expect_error(pretty_bytes(list(1,2,3)), 'is.numeric.*is not TRUE')

})

test_that("pretty_bytes converts properly", {

  expect_equal(pretty_bytes(0), '0 B')
  expect_equal(pretty_bytes(10), '10 B')
  expect_equal(pretty_bytes(999), '999 B')
  expect_equal(pretty_bytes(1001), '1 kB')
  expect_equal(pretty_bytes(1e16), '10 PB')
  expect_equal(pretty_bytes(1e30), '1000000 YB')

})

test_that("pretty_bytes handles NA and NaN", {

  expect_equal(pretty_bytes(NA_real_), "NA B")
  expect_equal(pretty_bytes(NA_integer_), "NA B")
  expect_error(pretty_bytes(NA_character_), 'is.numeric.*is not TRUE')
  expect_error(pretty_bytes(NA), 'is.numeric.*is not TRUE')

  expect_equal(pretty_bytes(NaN), "NaN B")

})

test_that("pretty_bytes handles vectors", {

  expect_equal(pretty_bytes(1:10), paste(format(1:10), "B"))
  v <- c(NA, 1, 1e4, 1e6, NaN, 1e5)

  expect_equal(pretty_bytes(v),
    c(" NA B", "  1 B", " 10 kB", "  1 MB", "NaN B", "100 kB"))

  expect_equal(pretty_bytes(numeric()), character())
})

test_that("pretty_bytes handles negative values", {
  v <- c(NA, -1, 1e4, 1e6, NaN, -1e5)
  expect_equal(pretty_bytes(v),
    c("  NA B", "  -1 B", "  10 kB", "   1 MB", " NaN B", "-100 kB"))

})
