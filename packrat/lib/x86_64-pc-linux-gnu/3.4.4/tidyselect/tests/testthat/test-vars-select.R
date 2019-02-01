context("select vars")

test_that("vars_select can rename variables", {
  vars <- c("a", "b")
  expect_equal(vars_select(vars, b = a, a = b), c("b" = "a", "a" = "b"))
})

test_that("last rename wins", {
  vars <- c("a", "b")

  expect_equal(vars_select(vars, b = a, c = a), c("c" = "a"))
})

test_that("negative index removes values", {
  vars <- letters[1:3]

  expect_equal(vars_select(vars, -c), c(a = "a", b = "b"))
  expect_equal(vars_select(vars, a:c, -c), c(a = "a", b = "b"))
  expect_equal(vars_select(vars, a, b, c, -c), c(a = "a", b = "b"))
  expect_equal(vars_select(vars, -c, a, b), c(a = "a", b = "b"))
})

test_that("can select with character vectors", {
  expect_identical(vars_select(letters, "b", !! "z", c("b", "c")), set_names(c("b", "z", "c")))
})

test_that("abort on unknown columns", {
  expect_error(vars_select(letters, "foo"), "Unknown column `foo`")
  expect_error(vars_select(letters, c("a", "bar", "foo", "d")), "`bar`")
})

test_that("symbol overscope is not isolated from context", {
  foo <- 10
  expect_identical(vars_select(letters, foo), c(j = "j"))
  expect_identical(vars_select(letters, ((foo))), c(j = "j"))
})

test_that("symbol overscope works with parenthesised expressions", {
  expect_identical(vars_select(letters, ((((a)):((w))))), vars_select(letters, a:w))
  expect_identical(vars_select(letters, -((((a)):((y))))), c(z = "z"))
})

test_that("can select with unnamed elements", {
  expect_identical(vars_select(c("a", ""), a), c(a = "a"))
  expect_identical(vars_select(c("a", NA), a), c(a = "a"))
  expect_identical(vars_select(c("", "a"), a), c(a = "a"))
  expect_identical(vars_select(c(NA, "a"), a), c(a = "a"))
})

test_that("can customise error messages", {
  vars <- structure(letters, type = c("variable", "variables"))

  expect_error(vars_select(vars, "foo"), "Unknown variable `foo`")
  expect_warning(vars_select(vars, one_of("bim")), "Unknown variables:")
  expect_error(vars_rename(vars, A = "foo"), "Unknown variable `foo`")
  expect_error(vars_pull(vars, !! c("a", "b")), "or a variable name")
})

test_that("can supply empty inputs", {
  empty_vars <- set_names(chr())
  expect_identical(vars_select(letters), empty_vars)
  expect_identical(vars_select(letters, NULL), empty_vars)
  expect_identical(vars_select(letters, chr()), empty_vars)

  expect_identical(vars_select(letters, a, NULL), c(a = "a"))
  expect_identical(vars_select(letters, a, chr()), c(a = "a"))
})

test_that("empty selection signals a condition", {
  expect_is(catch_cnd(vars_select(letters)), "tidyselect_empty_dots")
  expect_is(catch_cnd(vars_select(letters, starts_with("1"))), "tidyselect_empty")
})

test_that("unknown variables errors are ignored if `.strict` is FALSE", {
  expect_identical(vars_select(letters, `_foo`, .strict = FALSE), set_names(chr()))
  expect_identical(vars_select(letters, a, `_foo`, .strict = FALSE), c(a = "a"))
  expect_identical(vars_select(letters, a, "_foo", .strict = FALSE), c(a = "a"))

  expect_identical(vars_select(letters, a, -`_foo`, .strict = FALSE), c(a = "a"))
  expect_identical(vars_select(letters, a, -"`_foo`", .strict = FALSE), c(a = "a"))

  expect_identical(vars_select(letters, c(a, `_foo`, c), .strict = FALSE), c(a = "a", c = "c"))
  expect_identical(vars_select(letters, c(a, "_foo", c), .strict = FALSE), c(a = "a", c = "c"))
})

test_that("`:` handles strings", {
  expect_identical(vars_select(letters, "b":"d"), vars_select(letters, b:d))
  expect_error(vars_select(letters, "b":"Z"), "Unknown column `Z`")
})

test_that("`-` handles strings", {
  expect_identical(vars_select(letters, -"c"), vars_select(letters, -c))
})

test_that("`-` handles positions", {
  expect_identical(vars_select(letters, 10 - 7), vars_select(letters, 3))
})

test_that("`-` handles character vectors (#35)", {
  expect_identical(vars_select(letters, - (!! letters[1:20])), vars_select(letters, -(1:20)))
  expect_error(vars_select(letters, - c("foo", "z", "bar")), "Unknown column `foo`")
})

test_that("can select `c` despite overscoped c()", {
  expect_identical(vars_select(letters, c), c(c = "c"))
})

test_that("vars_select() handles named character vectors", {
  expect_identical(vars_select(letters, c("A" = "y", "B" = "z")), vars_select(letters, A = y, B = z))
  expect_identical(vars_select(letters, !! c("A" = "y", "B" = "z")), vars_select(letters, A = y, B = z))
})

test_that("can select with length > 1 double vectors (#43)", {
  expect_identical(vars_select(letters, !!c(1, 2)), c(a = "a", b = "b"))
})
