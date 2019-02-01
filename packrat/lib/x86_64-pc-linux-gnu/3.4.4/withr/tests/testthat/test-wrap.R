context("wrap")


test_that("wrap works", {
  v <- c(0, 0, 0)
  set <- function(x) v[2] <<- x
  f <- wrap(set, v[1] <<- v[1] + 1, v[3] <<- v[3] + 3)
  expect_equal(v, c(0, 0, 0))
  f(2)
  expect_equal(v, 1:3)
})
