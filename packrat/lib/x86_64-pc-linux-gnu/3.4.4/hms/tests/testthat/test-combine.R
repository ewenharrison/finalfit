context("combine")

test_that("combination keeps class and order", {
  expect_identical(c(hms(1), hms(2)), hms(1:2))
})

test_that("combination coerces to hms", {
  expect_identical(c(hms(1), 2), hms(1:2))
  if (getRversion() < "3.3") skip("Only for R >= 3.3")
  expect_identical(c(hms(1), "00:00:02"), hms(1:2))
})


# In R base,`c(as.difftime("20:00:00"), NA)` fails
test_that("composition with NA fails", {
  if (getRversion() < "3.3") skip("Only for R >= 3.3")
  expect_error(c(hms(1), NA))
})
