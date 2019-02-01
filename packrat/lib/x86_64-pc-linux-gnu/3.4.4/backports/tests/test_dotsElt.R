source("helper/helper.R")

wb = function(n, ...) backports:::...elt(n)

if (exists("...elt", envir = baseenv())) {
  f = get("...elt", envir = baseenv())
  wf = function(n, ...) f(n)
  expect_same = makeCompareFun(wf, wb)

  expect_same(1, 1, 2, 3)
  expect_same(2, 1, 2, 3)
  expect_same(3, 1, 2, 3)
}

expect_identical(wb(1, "a", "b", "c"), "a")
expect_identical(wb(2, "a", "b", "c"), "b")
expect_identical(wb(3, "a", "b", "c"), "c")
expect_error(wb(0, "a"), "non-positive")
expect_error(wb(2, "a"), "does not contain")
