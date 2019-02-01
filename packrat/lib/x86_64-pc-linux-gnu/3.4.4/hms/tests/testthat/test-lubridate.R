context("lubridate")

test_that("duration", {
  skip_if_not_installed("lubridate")
  expect_identical(lubridate::as.duration(hms(minutes = 1:3)),
                   lubridate::duration(minutes = 1:3))
})

test_that("interval", {
  skip_if_not_installed("lubridate")
  timestamp <- Sys.time()
  expect_identical(lubridate::as.interval(hms(seconds = 2), timestamp),
                   lubridate::interval(timestamp, timestamp + 2))
})

test_that("period", {
    skip_if_not_installed("lubridate")
    expect_identical(lubridate::as.period(hms(hours = -1)),
                     lubridate::period(hours = -1))
})
