context("Test hash/hmac functions")

test_that("Hash functions match openssl command line tool", {

  # COMPARE: echo -n "foo" | openssl dgst -md4
  expect_that(unclass(md4("foo")), equals("0ac6700c491d70fb8650940b1ca1e4b2"))
  expect_that(unclass(md5("foo")), equals("acbd18db4cc2f85cedef654fccc4a4d8"))
  expect_that(unclass(ripemd160("foo")), equals("42cfa211018ea492fdee45ac637b7972a0ad6873"))
  expect_that(unclass(sha1("foo")), equals("0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"))
  expect_that(unclass(sha256("foo")), equals("2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"))
  expect_that(unclass(sha512("foo")), equals("f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7"))
})

test_that("HMAC functions match openssl command line tool", {

  # #COMPARE: echo -n "foo" | openssl dgst -md4 -hmac "secret"
  expect_that(unclass(md4("foo", key = "secret")), equals("93e81ded7aec4ec0d73a97bb4792742a"))
  expect_that(unclass(md5("foo", key = "secret")), equals("ba19fbc606a960051b60244e9a5ed3d2"))
  expect_that(unclass(ripemd160("foo", key = "secret")), equals("a87093c26e44fdfa04e142e59710daa94556a5ed"))
  expect_that(unclass(sha1("foo", key = "secret")), equals("9baed91be7f58b57c824b60da7cb262b2ecafbd2"))
  expect_that(unclass(sha256("foo", key = "secret")), equals("773ba44693c7553d6ee20f61ea5d2757a9a4f4a44d2841ae4e95b52e4cd62db4"))
  expect_that(unclass(sha512("foo", key = "secret")), equals("82df7103de8d82de45e01c45fe642b5d13c6c2b47decafebc009431c665c6fa5f3d1af4e978ea1bde91426622073ebeac61a3461efd467e0971c788bc8ebdbbe"))
})

test_that("Connection interface matches raw interface", {
  mydata <- serialize(iris, NULL)
  saveRDS(iris, tmp <- tempfile())
  expect_equal(md5(mydata), md5(file(tmp)))
  expect_equal(sha1(mydata), sha1(file(tmp)))
  expect_equal(sha256(mydata), sha256(file(tmp)))
  expect_equal(md5(mydata, key = "secret"), md5(file(tmp), key = "secret"))
  expect_equal(sha1(mydata, key = "secret"), sha1(file(tmp), key = "secret"))
  expect_equal(sha256(mydata, key = "secret"), sha256(file(tmp), key = "secret"))
})

test_that("Connection interface matches string interface", {
  expect_equal(md5(charToRaw("foo")), md5(textConnection("foo")))
  expect_equal(sha1(charToRaw("foo")), sha1(textConnection("foo")))
  expect_equal(sha256(charToRaw("foo")), sha256(textConnection("foo")))
  expect_equal(md5(charToRaw("foo"), key = "secret"), md5(textConnection("foo"), key = "secret"))
  expect_equal(sha1(charToRaw("foo"), key = "secret"), sha1(textConnection("foo"), key = "secret"))
  expect_equal(sha256(charToRaw("foo"), key = "secret"), sha256(textConnection("foo"), key = "secret"))
})
