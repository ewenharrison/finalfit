source("helper/helper.R")

if (exists("file.mode", envir = baseenv())) {
  f = get("file.mode", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::file.mode)

  expect_same(R.home())
  expect_same(file.path(R.home(), "COPYING"))
}
