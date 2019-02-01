source("helper/helper.R")

wb = function(...) backports:::...length()

if (exists("...length", envir = baseenv())) {
  f = get("...length", envir = baseenv())
  wf = function(...) f()
  expect_same = makeCompareFun(wf, wb)

  expect_same(1)
  expect_same(1, 2)
  expect_same()
}

expect_identical(wb(1, "a", "b", "c"), 4L)
expect_identical(wb(2, "a", "b"), 3L)
expect_identical(wb(1), 1L)
expect_identical(wb(), 0L)

f = function(n) backports:::...length()
expect_error(f(), "current call")
expect_error(f(1), "current call")
