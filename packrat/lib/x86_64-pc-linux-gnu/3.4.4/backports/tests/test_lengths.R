source("helper/helper.R")

if (exists("lengths", envir = baseenv())) {
  f = get("lengths", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::lengths)

  expect_same(1:3)
  expect_same(setNames(1:3, letters[1:3]))
  expect_same(setNames(1:3, letters[1:3]), use.names = FALSE)
  expect_same(iris)
}
