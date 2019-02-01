context("update")

test_that("Can't update units", {
  x <- hms(minutes = 3)
  expect_equal(units(x), "secs")
  expect_warning(units(x) <- "mins", "always uses seconds")
  expect_equal(units(x), "secs")
  expect_warning(units(x) <- "secs", NA)
  expect_equal(units(x), "secs")
})
