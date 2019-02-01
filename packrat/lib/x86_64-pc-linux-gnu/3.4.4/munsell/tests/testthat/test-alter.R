
context("Lightening/darkening colours")

test_that("Lightening a light colour gives white", {
  expect_equal(lighter("5PB 9/4"), "N 10/0")
  expect_equal(lighter("N 9/0"), "N 10/0")
  expect_equal(lighter("N 10/0 "), "N 10/0")
  expect_equal(lighter(c("N 9/0 ", "N 10/0 ")), c("N 10/0", "N 10/0"))
})
  
test_that("Darkening a dark colour gives black", {
  expect_equal(darker("N 0/0"), "N 0/0")
  expect_equal(darker("5PB 1/4"), "N 0/0")
})

test_that("Negative darkening lightens", {
  expect_equal(lighter("5PB 2/4", -1), darker("5PB 2/4", 1))
})

context("Saturate/desaturate colours")

test_that("Saturation edge cases", {
  expect_equal(desaturate("5PB 2/2"), "N 2/0")
  expect_equal(saturate("5PB 2/32"), "5PB 2/34")
})

test_that("Saturation/desaturate opposites", {
  expect_equal(desaturate("5PB 2/4"), saturate("5PB 2/4", -1))
  expect_equal(desaturate(saturate("5PB 2/10")), "5PB 2/10")
})

context("Complement colours")

test_that("Complement",{
  expect_equal(complement("2.5R 2/2"), "2.5BG 2/2")
  expect_equal(complement("10G 2/2"), "10RP 2/2")
  expect_warning(complement("N 10/0"), "grey")
})

context("Hues")

test_that("hue edges",{
  expect_equal(pbgyr("2.5R 2/2"), "10RP 2/2")
  expect_equal(rygbp("10RP 2/2"), "2.5R 2/2")
  expect_warning(rygbp("N 10/0"), "[Gg]rey")
})

context("Handling NAs")
test_that(" NA handler", {
expect_equivalent(na_handle(na.exclude(NA), numeric(0)), as.numeric(NA))
# vector vector
expect_equal(na_handle(na.exclude(c(NA, 1:2)), 3:4), c(NA, 3, 4))
# vector dataframe
expect_equivalent(na_handle(na.exclude(c(NA, 1:2)), data.frame(x = 1:2, y = 4:5)),
  data.frame(x = c(NA, 1:2), y = c(NA, 4:5)))
# dataframe vector
expect_equivalent(na_handle(na.exclude(data.frame(x = c(NA, 1:2), y = c(NA, 4:5))), 1:2),
  c(NA, 1:2))
# dataframe dataframe 
expect_equivalent(na_handle(na.exclude(data.frame(x = c(NA, 1:2), y = c(NA, 4:5))), 
    data.frame(x = 1:2, y = 4:5)),
  data.frame(x = c(NA, 1:2), y = c(NA, 4:5)))
})

test_that("single NA", {
  expect_error(lighter(NA), "zero")
  expect_error(saturate(NA),"zero")
  expect_error(rygbp(NA), "zero")
  expect_error(complement(NA), "zero")
})

test_that("NA with colour", {
  expect_equal(lighter(c(NA, "10RP 2/2")), c(NA, lighter("10RP 2/2")))
  expect_equal(saturate(c(NA, "10RP 2/2")), c(NA, saturate("10RP 2/2")))
  expect_equal(rygbp(c(NA, "10RP 2/2")), c(NA, rygbp("10RP 2/2")))
  expect_equal(complement(c(NA, "10RP 2/2")), c(NA, complement("10RP 2/2")))
})

