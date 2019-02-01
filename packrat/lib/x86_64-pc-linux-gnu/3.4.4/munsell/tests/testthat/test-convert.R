context("Testing missing value conversion")

test_that("NAs handled in convert", {  
  expect_error(mnsl2hvc(c(NA)), "zero")
  expect_equal(hvc2mnsl(mnsl2hvc(c(NA, "10RP 2/2"))), c(NA, "10RP 2/2"))
})

test_that("NAs handled in checks", {
  expect_equal(check_mnsl(NA), as.character(NA))
  expect_equal(in_gamut(NA), as.logical(NA)) #wtf
  expect_equal(check_mnsl(c(NA, "10RP 2/2")), c(NA, "10RP 2/2"))
})

context("Out of gamut colors")

test_that("Fix gets passed along to `in_gamut()`", {
  expect_equal(mnsl("5PB 5/10", fix = TRUE), mnsl("5PB 5/10"))
  expect_equal(mnsl("5PB 5/14", fix = TRUE), mnsl("5PB 5/12"))
})

test_that("Converting out of gamut colors generate warnings", {
  expect_warning(mnsl("5PB 5/14"), "fix")
  expect_warning(mnsl(rygbp("2.5G 8/12")), "fix")
  
})

test_that("Altering out of gamut colors don't generate warnings", {
  expect_equal(lighter("5R 6/12"), "5R 7/12")
  expect_equal(rygbp("2.5G 8/12"), "5G 8/12")
  expect_equal(complement("2.5RP 8/12"), "2.5G 8/12")
})