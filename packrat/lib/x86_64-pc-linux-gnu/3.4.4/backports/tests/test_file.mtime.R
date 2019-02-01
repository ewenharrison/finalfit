source("helper/helper.R")

if (exists("file.mtime", envir = baseenv())) {
  f = get("file.mtime", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::file.mtime)

  expect_same(R.home())
  expect_same(file.path(R.home(), "COPYING"))
}
