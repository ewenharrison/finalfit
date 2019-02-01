source("helper/helper.R")

if (exists("trimws", envir = baseenv())) {
  f = get("trimws", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::trimws)

  expect_same("")
  expect_same(NA)
  expect_same(NA_character_)
  expect_same(sprintf(" %s ", letters))
  expect_same(" x ")
  expect_same(" x ", which = "both")
  expect_same(" x ", which = "left")
  expect_same(" x ", which = "right")
  expect_same(1)
}
