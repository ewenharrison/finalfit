source("helper/helper.R")

if (exists("file.size", envir = baseenv())) {
  f = get("file.size", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::file.size)

  expect_same(R.home())
  expect_same(file.path(R.home(), "COPYING"))
}
