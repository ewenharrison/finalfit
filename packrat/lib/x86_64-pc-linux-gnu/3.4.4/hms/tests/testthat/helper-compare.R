expect_hms_equal <- function(x, y) {
  expect_is(x, "hms")
  expect_is(y, "hms")
  expect_equal(as.numeric(x), as.numeric(y))
}

expect_difftime_equal <- function(x, y) {
  expect_is(x, "difftime")
  expect_is(y, "difftime")
  expect_equal(as.numeric(as.hms(x)), as.numeric(as.hms(y)))
}
