context("vars")

test_that("scoped_vars() restores previous state", {
  vars <- c("a", "b", "c")
  scoped_vars(vars)

  fn <- function() {
    scoped_vars(c("d", "e", "f"))
    starts_with("e")
  }
  expect_identical(fn(), 2L)

  expect_identical(peek_vars(), vars)
})

test_that("with_vars() works", {
  vars <- c("a", "b", "c")
  scoped_vars(vars)

  fn <- function(expr) {
    with_vars(c("rose", "blue", "red"), expr)
  }
  expect_identical(fn(starts_with("r")), c(1L, 3L))

  expect_identical(peek_vars(), vars)
})

test_that("has_vars() detects variables", {
  expect_false(has_vars())

  scoped_vars(letters)
  expect_true(has_vars())
})

test_that("Missing names are kept", {
  scoped_vars(c("foo", NA))
  expect_identical(peek_vars(), c("foo", NA))

  scoped_vars(c(NA, "foo"))
  expect_identical(peek_vars(), c(NA, "foo"))

  scoped_vars(c("bar", ""))
  expect_identical(peek_vars(), c("bar", ""))

  scoped_vars(c("", "bar"))
  expect_identical(peek_vars(), c("", "bar"))
})
