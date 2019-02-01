context("rename vars")

test_that("when .strict = FALSE, vars_rename always succeeds", {
  expect_error(
    vars_rename(c("a", "b"), d = e, .strict = TRUE),
    "object 'e' not found",
    fixed = TRUE
  )

  expect_error(
    vars_rename(c("a", "b"), d = e, f = g, .strict = TRUE),
    "object 'e' not found",
    fixed = TRUE
  )

  expect_equal(
    vars_rename(c("a", "b"), d = e, .strict = FALSE),
    c("a" = "a", "b" = "b")
  )

  expect_identical(
    vars_rename("x", A = x, B = y, .strict = FALSE),
    c(A = "x")
  )

  expect_error(
    vars_rename(c("a", "b"), d = "e", f = "g", .strict = TRUE),
    "Unknown columns `e` and `g`",
    fixed = TRUE
  )

  expect_identical(
    vars_rename("x", A = "x", B = "y", .strict = FALSE),
    c(A = "x")
  )
})

test_that("vars_rename() works with positions", {
  expect_identical(vars_rename(letters[1:4], new1 = 2, new2 = 4), c(a = "a", new1 = "b", c = "c", new2 = "d"))
  expect_error(vars_rename(letters, new = 1.5), "Column positions must be round numbers")
})

test_that("vars_rename() expects symbol or string", {
  expect_error(
    vars_rename(letters, d = !! list()),
    '`d` = list() must be a column name or position, not a list',
    fixed = TRUE
  )
})

test_that("vars_rename() sets variable context", {
  expect_identical(vars_rename(c("a", "b"), B = one_of("b")), c(a = "a", B = "b"))
})

test_that("vars_rename() fails with vectors", {
  expect_error(vars_rename(letters, A = 1:2), "Column positions must be scalar")
})

test_that("vars_rename() supports `.data` pronoun", {
  expect_identical(vars_rename(c("a", "b"), B = .data$b), c(a = "a", B = "b"))
})

test_that("vars_rename() unquotes named character vectors", {
  vars <- c(foo = "a", bar = "z")
  expect_identical(vars_rename(letters, !!! vars), vars_rename(letters, foo = a, bar = z))
  expect_identical(vars_rename(letters, !! vars), vars_rename(letters, foo = a, bar = z))
})
