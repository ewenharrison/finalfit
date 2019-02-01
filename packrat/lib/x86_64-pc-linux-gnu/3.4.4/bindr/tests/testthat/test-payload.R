context("payload")

test_that("create_env() with payload", {
  env <- create_env(lapply(letters, as.name), paste, "letter")
  expect_equal(env$a, "a letter")
  expect_equal(env$x, "x letter")
  expect_null(env$X)
  expect_equal(length(ls(env)), length(letters))
  expect_error(env$a <- "a", "read-only")
})

test_that("create_env() with named payload", {
  env <- create_env(lapply(letters, as.name), paste0, 1:3, collapse = "")
  expect_equal(env$a, "a1a2a3")
  expect_equal(env$x, "x1x2x3")
  expect_null(env$X)
  expect_equal(length(ls(env)), length(letters))
  expect_error(env$a <- "a", "read-only")
})
