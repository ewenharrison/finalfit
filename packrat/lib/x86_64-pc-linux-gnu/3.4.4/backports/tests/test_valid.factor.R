source("helper/helper.R")

f = backports:::.valid.factor
x = factor(letters[1:3])
expect_identical(f(x), TRUE)

x = 1:2
attr(x, "levels") = c("a", "a")
expect_true(is.character(.valid.factor(x)))

if (exists(".valid.factor", envir = baseenv())) {
  f = get(".valid.factor", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::.valid.factor)

  expect_same(letters[1:2])
  expect_same(NULL)
  expect_same(iris)
  expect_same(factor("a"))
  expect_same(factor(1:3, levels = 1:3))

  x = 1:2
  class(x) = "factor"
  attr(x, "levels") = c("a", "b")
  expect_same(x)

  attr(x, "levels") = c("a", "a")
  expect_same(x)

  attr(x, "levels") = 1:2
  expect_same(x)
}
