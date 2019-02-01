context("output")

test_that("output", {
  expect_identical(format(hms(1:2, minutes = c(0, 0), hours = 3:4)),
                   c("03:00:01", "04:00:02"))
  expect_identical(format(hms(minutes = 1:-1)),
                   c(" 00:01:00", " 00:00:00", "-00:01:00"))
  expect_output(
    expect_identical(print(hms(minutes = 1:2, hours = 3:4)),
                     hms(minutes = 1:2, hours = 3:4)),
    "03:01:00\n04:02:00", fixed = TRUE)
})

test_that("beyond 24 hours (#12)", {
  expect_identical(format(hms(hours = 23:25)),
                   c("23:00:00", "24:00:00", "25:00:00"))
  expect_identical(format(hms(hours = 99:101)),
                   c(" 99:00:00", "100:00:00", "101:00:00"))
  expect_identical(format(hms(hours = c(-99, 100))),
                   c("- 99:00:00", " 100:00:00"))
  expect_identical(format(hms(hours = c(-100, 99))),
                   c("-100:00:00", "  99:00:00"))
})

test_that("fractional seconds (#13)", {
  expect_identical(format(hms(0.1)),
                   c("00:00:00.1"))
  expect_identical(format(hms(c(12, 0.3))),
                   c("00:00:12.0", "00:00:00.3"))
  expect_identical(format(hms(c(0.1, 0.01))),
                   c("00:00:00.10", "00:00:00.01"))
  expect_identical(format(hms(c(12, 0.3), minutes = c(0, 0), hours = c(345, 6))),
                   c("345:00:12.0", " 06:00:00.3"))
  expect_identical(format(hms(c(-0.1, 0.1))),
                   c("-00:00:00.1", " 00:00:00.1"))
})

test_that("picoseconds (#17)", {
  expect_identical(format(hms(1e-6)),
                   c("00:00:00.000001"))
  expect_identical(format(hms(9e-7)),
                   c("00:00:00.000001"))
  expect_identical(format(hms(4e-7)),
                   c("00:00:00.000000"))
  expect_identical(format(hms(1e-10)),
                   c("00:00:00.000000"))
  expect_identical(format(hms(1e-20)),
                   c("00:00:00.000000"))
  expect_identical(format(hms(c(1, 1e-20))),
                   c("00:00:01.000000", "00:00:00.000000"))
})

test_that("NA", {
  expect_identical(format(hms(NA)),
                   c("NA"))
})
