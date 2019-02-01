context("construct")

test_that("constructor", {
  expect_identical(hms(1:3, 2:4, 3:5, 4:6),
                   hms(seconds = 1:3 + 2:4 * 60 + 3:5 * 3600 + 4:6 * 86400))
  expect_identical(hms(-1, 1), hms(59))
  expect_identical(hms(3600), hms(hours = 1))

  expect_equal(length(hms(1)), 1L)
  expect_true(is.hms(hms(1)))
  expect_is(hms(1), "difftime")
  expect_identical(as.numeric(hms(1)), 1)
  expect_identical(as.difftime(hms(1)), hms(1))

  expect_identical(units(as.hms(as.difftime(1, units = "mins"))), "secs")
  expect_identical(as.hms(hms(1)), hms(1))
})

test_that("zero length (#35)", {
  expect_equal(length(hms()), 0L)
  expect_true(is.hms(hms()))
  expect_is(hms(), "difftime")
  expect_identical(as.numeric(hms()), numeric())
  expect_identical(as.difftime(hms()), hms())

  expect_identical(hms(), hms(seconds = numeric()))
  expect_identical(hms(), hms(minutes = numeric()))
  expect_identical(hms(), hms(hours = numeric()))
  expect_identical(hms(), hms(days = numeric()))
  expect_identical(hms(), as.hms(numeric()))
})

test_that("bad input", {
  expect_error(hms(hours = 1, seconds = 3), "only")
  expect_error(hms(minutes = 1, days = 3), "only")
  expect_error(hms(minutes = 1, hours = 2:3), "same length or be NULL")
  expect_error(hms(seconds = 1:5, minutes = 6:10, hours = 11:17), "same length or be NULL")
  expect_error(hms("05:00"), "must be numeric")
})
