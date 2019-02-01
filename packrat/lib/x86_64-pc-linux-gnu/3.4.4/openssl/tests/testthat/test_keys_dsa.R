context("Test DSA formats")

# Read secret key and public key
sk1 <- read_key("../keys/id_dsa")
pk1 <- read_pubkey("../keys/id_dsa.pub")

test_that("reading protected keys", {
  sk2 <- read_key("../keys/id_dsa.pw", password = "test")
  expect_equal(sk1, sk2)
  expect_error(read_key("../keys/id_dsa.pw", password = ""), "bad password")
})

test_that("reading public key formats", {
  pk2 <- read_pubkey("../keys/id_dsa.pem")
  pk3 <- read_pubkey("../keys/id_dsa.pub")
  pk4 <- as.list(sk1)$pubkey
  expect_equal(pk1, pk2)
  expect_equal(pk1, pk3)
  expect_equal(pk1, pk4)
})

test_that("pubkey ssh fingerprint", {
  fp <- paste(as.list(pk1)$fingerprint, collapse = "")
  expect_equal(fp, "6c42ea8454e549b855cadd7fc86609ca")
  pk5 <- read_pubkey(readLines("../keys/authorized_keys")[1])
  expect_equal(pk1, pk5)
  pk6 <- read_pubkey(write_ssh(pk1))
  expect_equal(pk1, pk6)
})

test_that("signatures", {
  # DSA does not support MD5
  msg <- readBin("../keys/message", raw(), 100)

  # SHA1 signature
  sig <- readBin("../keys/message.sig.dsa.sha1", raw(), 1000)
  expect_true(signature_verify(msg, sig, sha1, pk1))

  sig <- signature_create(msg, sha1, sk1)
  expect_true(signature_verify(msg, sig, sha1, pk1))

  # SHA256 signature
  sig <- readBin("../keys/message.sig.dsa.sha256", raw(), 1000)
  expect_true(signature_verify(msg, sig, sha256, pk1))

  sig <- signature_create(msg, sha256, sk1)
  expect_true(signature_verify(msg, sig, sha256, pk1))
})

test_that("roundtrip pem format", {
  expect_equal(pk1, read_pubkey(write_pem(pk1)))
  expect_equal(sk1, read_key(write_pem(sk1, password = NULL)))
  expect_equal(pk1, read_pubkey(write_pem(pk1, tempfile())))
  expect_equal(sk1, read_key(write_pem(sk1, tempfile(), password = NULL)))
})

test_that("roundtrip der format", {
  expect_equal(pk1, read_pubkey(write_der(pk1), der = TRUE))
  expect_equal(sk1, read_key(write_der(sk1), der = TRUE))
  expect_equal(pk1, read_pubkey(write_der(pk1, tempfile()), der = TRUE))
  expect_equal(sk1, read_key(write_der(sk1, tempfile()), der = TRUE))
})

test_that("signature path interface", {
  sig <- signature_create("../keys/message", sha256, "../keys/id_dsa")
  writeBin(sig, tmp <- tempfile())
  expect_true(signature_verify("../keys/message", tmp, sha256, "../keys/id_dsa.pub"))
})

test_that("dsa_keygen works", {
  key <- dsa_keygen(1024)
  expect_is(key, "dsa")
  expect_equal(as.list(key)$size, 1024)
  rm(key)

  key <- dsa_keygen(2048)
  expect_is(key, "dsa")
  expect_equal(as.list(key)$size, 2048)
  rm(key)
})

# Cleanup
rm(sk1, pk1)
