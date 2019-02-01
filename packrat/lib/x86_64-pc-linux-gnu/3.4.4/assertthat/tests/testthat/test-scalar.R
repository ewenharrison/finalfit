context("Scalar assertions")

test_that("is.scalar works correctly", {
  expect_true(is.scalar(1))
  expect_true(is.scalar(-1))
  expect_true(is.scalar(1.5))
  expect_false(is.scalar(1:5))
  expect_true(is.scalar('a'))
  expect_false(is.scalar(c('a', 'b')))
  expect_true(is.scalar(TRUE))
  expect_false(is.scalar(c(TRUE, FALSE)))
  expect_false(is.scalar(NULL))
  expect_true(is.scalar(NA))
})

test_that("is.string works correctly", {
  expect_false(is.string(1))
  expect_true(is.string('a'))
  expect_false(is.string(c('a', 'b')))
  expect_false(is.string(TRUE))
  expect_false(is.string(NULL))
  expect_false(is.string(NA))
})

test_that("is.number works correctly", {
  expect_true(is.number(1))
  expect_true(is.number(-1))
  expect_true(is.number(1.5))
  expect_false(is.number(1:5))
  expect_false(is.number('a'))
  expect_false(is.number(TRUE))
  expect_false(is.number(NULL))
  expect_false(is.number(NA))
})

test_that("is.flag works correctly", {
  expect_false(is.flag(1))
  expect_false(is.flag('a'))
  expect_true(is.flag(TRUE))
  expect_true(is.flag(FALSE))
  expect_false(is.flag(c(TRUE, FALSE)))
  expect_false(is.flag(NULL))
  expect_equal(is.flag(NA), is.logical(NA)) # not obvious
})

test_that("is.count works correctly", {
  expect_true(is.count(1))
  expect_false(is.count(-1))
  expect_false(is.count(1.5))
  expect_false(is.count(1:5))
  expect_false(is.count('a'))
  expect_false(is.count(TRUE))
  expect_false(is.count(NULL))
  expect_false(is.count(NA))
})
