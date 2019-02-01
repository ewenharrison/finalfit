context("round")

test_that("round_hms", {
  expect_equal(round_hms(parse_hms("12:34:56"), 5), hms(55, 34, 12))
  expect_equal(round_hms(parse_hms("12:34:56"), 60), hms(0, 35, 12))
  expect_equal(round_hms(hms(0.7), 0.25), hms(0.75))
  expect_equal(round_hms(hms(NA), 5), hms(NA))
  expect_equal(round_hms(parse_hms(c("12:34:56", NA)), 5), as.hms(c(hms(55, 34, 12), hms(NA))))
})

test_that("trunc_hms", {
  expect_equal(trunc_hms(parse_hms("12:34:56"), 5), hms(55, 34, 12))
  expect_equal(trunc_hms(parse_hms("12:34:56"), 60), hms(0, 34, 12))
  expect_equal(trunc_hms(hms(0.7), 0.25), hms(0.5))
  expect_equal(trunc_hms(hms(NA), 5), hms(NA))
  expect_equal(trunc_hms(parse_hms(c("12:34:56", NA)), 5), as.hms(c(hms(55, 34, 12), hms(NA))))
})
