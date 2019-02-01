context("populate")

test_that("can populate existing env", {
  env <- new.env(parent = emptyenv())
  populate_env(env, letters, identity)
  expect_equal(env$a, quote(a))
  expect_equal(env$k, quote(k))
  expect_null(env$Z)
})

test_that("cannot update existing vars", {
  env <- new.env(parent = emptyenv())
  populate_env(env, "v", identity)
  expect_error(populate_env(env, letters, identity), "existing")
})
