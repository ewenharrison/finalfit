source("helper/helper.R")

if (exists("dir.exists", envir = baseenv())) {
  f = get("dir.exists", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::dir.exists)

  expect_same(tempdir())
  expect_same(tempfile())
  expect_same(rep.int(tempdir(), 2))
  expect_same(TRUE)
}
