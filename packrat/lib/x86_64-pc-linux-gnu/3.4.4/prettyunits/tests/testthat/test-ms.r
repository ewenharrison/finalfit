
context("Pretty milliseconds")

test_that("pretty_ms works", {

  expect_equal(pretty_ms(0), '0ms')
  expect_equal(pretty_ms(0.1), '1ms')
  expect_equal(pretty_ms(1), '1ms')
  expect_equal(pretty_ms(1000 + 400), '1.4s')
  expect_equal(pretty_ms(1000 * 2 + 400), '2.4s')
  expect_equal(pretty_ms(1000 * 55), '55s')
  expect_equal(pretty_ms(1000 * 67), '1m 7s')
  expect_equal(pretty_ms(1000 * 60 * 5), '5m')
  expect_equal(pretty_ms(1000 * 60 * 67), '1h 7m')
  expect_equal(pretty_ms(1000 * 60 * 60 * 12), '12h')
  expect_equal(pretty_ms(1000 * 60 * 60 * 40), '1d 16h')
  expect_equal(pretty_ms(1000 * 60 * 60 * 999), '41d 15h')

})

test_that("compact pretty_ms works", {

  expect_equal(pretty_ms(1000 + 4, compact = TRUE), '~1s')
  expect_equal(pretty_ms(1000 * 60 * 60 * 999, compact =  TRUE), '~41d')

})

test_that("pretty_ms handles vectors", {

  v <- c(0, 0.1, 1, 1400, 2400, 1000 * 55, 1000 * 67,
         1000 * 60 * 5, 1000 * 60 * 67, 1000 * 60 * 60 * 12,
         1000 * 60 * 60 * 40, 1000 * 60 * 60 * 999)
  v2 <- c("0ms", "1ms", "1ms", "1.4s", "2.4s", "55s", "1m 7s",
          "5m", "1h 7m", "12h", "1d 16h", "41d 15h")

  expect_equal(pretty_ms(v), v2)
})

context("Pretty seconds")

test_that("pretty_sec works", {

  expect_equal(pretty_sec(0 / 1000), '0ms')
  expect_equal(pretty_sec(0.1 / 1000), '1ms')
  expect_equal(pretty_sec(1 / 1000), '1ms')
  expect_equal(pretty_sec((1000 + 400) / 1000), '1.4s')
  expect_equal(pretty_sec((1000 * 2 + 400) / 1000), '2.4s')
  expect_equal(pretty_sec(1000 * 55 / 1000), '55s')
  expect_equal(pretty_sec(1000 * 67 / 1000), '1m 7s')
  expect_equal(pretty_sec(1000 * 60 * 5 / 1000), '5m')
  expect_equal(pretty_sec(1000 * 60 * 67 / 1000), '1h 7m')
  expect_equal(pretty_sec(1000 * 60 * 60 * 12 / 1000), '12h')
  expect_equal(pretty_sec(1000 * 60 * 60 * 40 / 1000), '1d 16h')
  expect_equal(pretty_sec(1000 * 60 * 60 * 999 / 1000), '41d 15h')

})

test_that("compact pretty_sec works", {

  expect_equal(pretty_sec((1000 + 4) / 1000, compact = TRUE), '~1s')
  expect_equal(pretty_sec(1000 * 60 * 60 * 999 / 1000, compact =  TRUE), '~41d')

})

test_that("pretty_sec handles vectors", {

  v <- c(0, 0.1, 1, 1400, 2400, 1000 * 55, 1000 * 67,
         1000 * 60 * 5, 1000 * 60 * 67, 1000 * 60 * 60 * 12,
         1000 * 60 * 60 * 40, 1000 * 60 * 60 * 999) / 1000
  v2 <- c("0ms", "1ms", "1ms", "1.4s", "2.4s", "55s", "1m 7s",
          "5m", "1h 7m", "12h", "1d 16h", "41d 15h")

  expect_equal(pretty_sec(v), v2)
})

context("Pretty dt")

test_that("pretty_dt works", {

  expect_equal(pretty_dt(as.difftime(units = "secs", 0 / 1000)), '0ms')
  expect_equal(pretty_dt(as.difftime(units = "secs", 0.1 / 1000)), '1ms')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1 / 1000)), '1ms')
  expect_equal(pretty_dt(as.difftime(units = "secs", (1000 + 400) / 1000)), '1.4s')
  expect_equal(pretty_dt(as.difftime(units = "secs", (1000 * 2 + 400) / 1000)), '2.4s')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 55 / 1000)), '55s')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 67 / 1000)), '1m 7s')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 5 / 1000)), '5m')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 67 / 1000)), '1h 7m')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 60 * 12 / 1000)), '12h')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 60 * 40 / 1000)), '1d 16h')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 60 * 999 / 1000)), '41d 15h')

})

test_that("compact pretty_dt works", {

  expect_equal(pretty_dt(as.difftime(units = "secs", (1000 + 4) / 1000), compact = TRUE), '~1s')
  expect_equal(pretty_dt(as.difftime(units = "secs", 1000 * 60 * 60 * 999 / 1000), compact =  TRUE), '~41d')

})

test_that("pretty_dt handles vectors", {

  v <- c(0, 0.1, 1, 1400, 2400, 1000 * 55, 1000 * 67,
         1000 * 60 * 5, 1000 * 60 * 67, 1000 * 60 * 60 * 12,
         1000 * 60 * 60 * 40, 1000 * 60 * 60 * 999) / 1000
  v2 <- c("0ms", "1ms", "1ms", "1.4s", "2.4s", "55s", "1m 7s",
          "5m", "1h 7m", "12h", "1d 16h", "41d 15h")

  expect_equal(pretty_dt(as.difftime(units = "secs", v)), v2)
})
