context("Test RSA encryption")

test_that("rsa decrypt", {
  key <- read_key("../keys/id_rsa")
  msg <- readBin("../keys/message", raw(), 1000)
  ct <- readBin("../keys/message.rsa.crypt", raw(), 1000)
  expect_equal(msg ,rsa_decrypt(ct, key))
})

test_that("encrypt with various rsa key sizes", {
  for(size in c(512, 1024, 2048, 4096)){
    key <- rsa_keygen(size)
    pubkey <- as.list(key)$pubkey
    msg <- rand_bytes(size / 10)
    ct <- rsa_encrypt(msg, pubkey)
    expect_equal(msg, rsa_decrypt(ct, key))
    bigmsg <- rand_bytes(size / 8)
    expect_error(rsa_encrypt(bigmsg, pubkey), "too large")
  }
})

test_that("envelopes with various rsa key sizes", {
  for(size in c(512, 1024, 2048, 4096)){
    key <- rsa_keygen(size)
    pubkey <- as.list(key)$pubkey
    msg <- serialize(iris, NULL)
    out <- encrypt_envelope(msg, pubkey)
    orig <- decrypt_envelope(out$data, out$iv, out$session, key)
    expect_equal(msg, orig)
  }
})
