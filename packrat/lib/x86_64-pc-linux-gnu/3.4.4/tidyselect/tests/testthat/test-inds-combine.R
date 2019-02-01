context("combine indices")

# This is the low C++ function which works on integer indices

test_that("empty index gives empty output", {
  vars <- inds_combine(letters, list())
  expect_equal(length(vars), 0)

  vars <- inds_combine(letters, list(numeric()))
  expect_equal(length(vars), 0)
})

test_that("positive indexes kept", {
  expect_equal(inds_combine(letters, list(1)), c(a = 1))
  expect_equal(inds_combine(letters, list(1, 26)), c(a = 1, z = 26))
  expect_equal(inds_combine(letters, list(c(1, 26))), c(a = 1, z = 26))
})

test_that("indexes returned in order they appear", {
  expect_equal(inds_combine(letters, list(26, 1)), c(z = 26, a = 1))
})


test_that("negative index in first position includes all others", {
  vars <- inds_combine(letters[1:3], list(-1))
  expect_equal(vars, c(b = 2, c = 3))
})

test_that("named inputs rename outputs", {
  expect_equal(inds_combine(letters[1:3], list(d = 1)), c(d = 1))
  expect_equal(inds_combine(letters[1:3], list(c(d = 1))), c(d = 1))
})

test_that("if multiple names, last kept", {
  expect_equal(inds_combine(letters[1:3], list(d = 1, e = 1)), c(e = 1))
  expect_equal(inds_combine(letters[1:3], list(c(d = 1, e = 1))), c(e = 1))
})

test_that("if one name for multiple vars, use integer index", {
  expect_equal(inds_combine(letters[1:3], list(x = 1:3)), c(x1 = 1, x2 = 2, x3 = 3))
})

test_that("invalid inputs raise error", {
  expect_error(
    inds_combine(names(mtcars), list(0)),
    "Each argument must yield either positive or negative integers",
    fixed = TRUE
  )
  expect_error(
    inds_combine(names(mtcars), list(c(-1, 1))),
    "Each argument must yield either positive or negative integers",
    fixed = TRUE
  )
  expect_error(
    inds_combine(names(mtcars), list(12)),
    "Position must be between 0 and n",
    fixed = TRUE
  )
})
