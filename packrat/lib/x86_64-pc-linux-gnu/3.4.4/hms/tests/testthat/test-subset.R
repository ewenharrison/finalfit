context("subset")

test_that("range subsetting keeps class", {
  expect_identical(hms(1:3)[2], hms(2))
  expect_identical(hms(1:3)[2:3], hms(2:3))
})

test_that("range updating keeps class", {
  x <- hms(1:3)
  x[2] <- 4
  expect_identical(x, hms(c(1,4,3)))
  x <- hms(1:4)
  x[2:3] <- 5:6
  expect_identical(x, hms(c(1,5,6,4)))
})

test_that("index subsetting keeps class", {
  expect_identical(hms(1:3)[[2]], hms(2))
})

test_that("index updating keeps class", {
  x <- hms(1:3)
  x[[2]] <- 4
  expect_identical(x, hms(c(1,4,3)))
})
