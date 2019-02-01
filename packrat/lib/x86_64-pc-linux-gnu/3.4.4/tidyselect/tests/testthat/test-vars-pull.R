context("pull var")

test_that("errors for bad inputs", {
  expect_error(
    vars_pull(letters, letters),
    "`var` must evaluate to a single number",
    fixed = TRUE
  )

  expect_error(
    vars_pull(letters, aa),
    "object 'aa' not found",
    fixed = TRUE
  )

  expect_error(
    vars_pull(letters, 0),
    "`var` must be a value between -26 and 26 (excluding zero), not 0",
    fixed = TRUE
  )
  expect_error(
    vars_pull(letters, 100),
    "`var` must be a value between -26 and 26 (excluding zero), not 100",
    fixed = TRUE
  )
  expect_error(
    vars_pull(letters, -Inf),
    "`var` must be a value between -26 and 26 (excluding zero), not NA",
    fixed = TRUE
  )
  expect_error(
    vars_pull(letters, NA_integer_),
    "`var` must be a value between -26 and 26 (excluding zero), not NA",
    fixed = TRUE
  )
})

test_that("can pull variables with missing elements", {
  expect_identical(vars_pull(c("a", ""), a), "a")
  expect_identical(vars_pull(c("a", NA), a), "a")
})
