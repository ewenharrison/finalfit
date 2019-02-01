context("Test salting works with various algorithms")

test_that("MD4 salts single values", {
  expect_false(md4("foo") == md4("foo","bar"))
})

test_that("MD4 salts multiple values", {
  salted_hashes <- md4(c("foo","bar"), "baz")
  unsalted_hashes <- md4(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})

test_that("MD5 salts single values", {
  expect_false(md5("foo") == md5("foo","bar"))
})

test_that("MD5 salts multiple values", {
  salted_hashes <- md5(c("foo","bar"), "baz")
  unsalted_hashes <- md5(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})

test_that("RIPEMD160 salts single values", {
  expect_false(ripemd160("foo") == md5("foo","bar"))
})

test_that("RIPEMD160 salts multiple values", {
  salted_hashes <- ripemd160(c("foo","bar"), "baz")
  unsalted_hashes <- ripemd160(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})

test_that("SHA1 salts single values", {
  expect_false(sha1("foo") == md5("foo","bar"))
})

test_that("SHA1 salts multiple values", {
  salted_hashes <- sha1(c("foo","bar"), "baz")
  unsalted_hashes <- sha1(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})

test_that("SHA256 salts single values", {
  expect_false(sha256("foo") == md5("foo","bar"))
})

test_that("SHA256 salts multiple values", {
  salted_hashes <- sha256(c("foo","bar"), "baz")
  unsalted_hashes <- sha256(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})

test_that("SHA512 salts single values", {
  expect_false(sha512("foo") == md5("foo","bar"))
})

test_that("SHA512 salts multiple values", {
  salted_hashes <- sha512(c("foo","bar"), "baz")
  unsalted_hashes <- sha512(c("foo","bar"))
  expect_that(length(salted_hashes), equals(2))
  expect_false(salted_hashes[1] == unsalted_hashes[1])
  expect_false(salted_hashes[2] == unsalted_hashes[2])
})
