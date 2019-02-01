source("helper/helper.R")

if (exists("anyNA", envir = baseenv())) {
  f = get("anyNA", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::anyNA)

  expect_same(1)
  expect_same(NA)
  expect_same(iris)
  if (getRversion() >= "3.2.0") {
    expect_same(list(1, 2, list(3, 4, list(NA))), recursive = FALSE)
    expect_same(list(1, 2, list(3, 4, list(NA))), recursive = TRUE)
  }
}
