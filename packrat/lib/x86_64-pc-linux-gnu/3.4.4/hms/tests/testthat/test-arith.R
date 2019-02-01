context("arith")

test_that("arithmetics work", {
  expect_equal(as.Date("2016-03-31") + hms(hours = 1), as.Date("2016-03-31"))
  expect_equal(as.Date("2016-03-31") + hms(days = -1), as.Date("2016-03-30"))
  expect_equal(as.POSIXct("2016-03-31") + hms(1), as.POSIXct("2016-03-31 00:00:01"))
  expect_equal(hms(hours = 1) + as.Date("2016-03-31"), as.Date("2016-03-31"))
  expect_equal(hms(days = 1) + as.Date("2016-03-31"), as.Date("2016-04-01"))
  expect_equal(hms(hours = 1) + as.POSIXct("2016-03-31"), as.POSIXct("2016-03-31 01:00:00"))

  expect_difftime_equal(hms(1) + hms(2), hms(3))
  expect_difftime_equal(hms(1) - hms(2), hms(-1))
  expect_difftime_equal(2 * hms(1), hms(2))
  expect_difftime_equal(hms(hours = 1) / 2, hms(minutes = 30))
})

test_that("component extraction work", {
  x <- hms(12.3, 45, 23, 1)
  expect_equal(split_second_of_second(x), 0.3)
  expect_equal(second_of_minute(x), 12)
  expect_equal(minute_of_hour(x), 45)
  expect_equal(hour_of_day(x), 23)
  expect_equal(days(x), 1)
})

test_that("component extraction work for negative times", {
  x <- -hms(12.3, 45, 23, 1)
  expect_equal(split_second_of_second(x), 0.3)
  expect_equal(second_of_minute(x), 12)
  expect_equal(minute_of_hour(x), 45)
  expect_equal(hour_of_day(x), 23)
  expect_equal(days(x), -1)
})
