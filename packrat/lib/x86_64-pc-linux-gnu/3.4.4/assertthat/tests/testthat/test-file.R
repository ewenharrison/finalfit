context("File assertions")

test_that("is.dir identifies dirs correctly", {
  expect_true(is.dir(tempdir()))
  expect_error(is.dir(tempfile()))
})

test_that("is.writeable works correctly", {
  expect_true(is.writeable(tempdir()))
  tf <- tempfile()
  expect_error(is.writeable(tf)) # file doesn't exist yet
  cat("foo", file=tf)
  expect_true(is.writeable(tf)) # ...but now it does
})

test_that("is.readable works correctly", {
  expect_true(is.readable(tempdir()))
  tf <- tempfile()
  expect_error(is.readable(tf)) # file doesn't exist yet
  cat("foo", file=tf)
  expect_true(is.readable(tf)) # ...but now it does
})

test_that("has_extension works correctly", {
  # no extension
  tf <- tempfile()
  expect_true(has_extension(tf, ""))
  expect_false(has_extension(tf, "x"))
         
  # normal extension
  ext <- "test"
  tf <- tempfile(fileext=paste0(".", ext))
  expect_true(has_extension(tf, ext))
  expect_false(has_extension(tf, paste0(ext, "x")))
  
  # empty extension
  ext <- ""
  tf <- tempfile(fileext=paste0(".", ext))
  expect_true(has_extension(tf, ext))
  expect_false(has_extension(tf, paste0(ext, "x")))
})
