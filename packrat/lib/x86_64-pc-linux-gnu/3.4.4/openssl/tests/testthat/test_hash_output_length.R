context("Test the length of the output of various algorithms.")

text <- "foo"
bin <- charToRaw(text)

test_that("MD4 outputs a 32-character hash", {

  expect_that(nchar(md4(text)), equals(32))
  expect_that(length(md4(bin)), equals(16))
  expect_identical(as.character(md4(bin)), md4(text))

})

test_that("MD5 outputs a 32-character hash", {

  expect_that(nchar(md5(text)), equals(32))
  expect_that(length(md5(bin)), equals(16))
  expect_identical(as.character(md5(bin)), md5(text))

})

test_that("ripemd160 outputs a 40-character hash", {

  expect_that(nchar(ripemd160(text)), equals(40))
  expect_that(length(ripemd160(bin)), equals(20))
  expect_identical(as.character(ripemd160(bin)), ripemd160(text))

})

test_that("SHA1 outputs a 40-character hash", {

  expect_that(nchar(sha1(text)), equals(40))
  expect_that(length(sha1(bin)), equals(20))
  expect_identical(as.character(sha1(bin)), sha1(text))

})

test_that("SHA256 outputs a 64-character hash", {

  expect_that(nchar(sha256(text)), equals(64))
  expect_that(length(sha256(bin)), equals(32))
  expect_identical(as.character(sha256(bin)), sha256(text))

})

test_that("SHA512 outputs a 128-character hash", {

  expect_that(nchar(sha512(text)), equals(128))
  expect_that(length(sha512(bin)), equals(64))
  expect_identical(as.character(sha512(bin)), sha512(text))

})
