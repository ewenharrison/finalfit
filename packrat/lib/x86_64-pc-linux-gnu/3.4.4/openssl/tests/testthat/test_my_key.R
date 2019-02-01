context("Test default user key")

# Read secret key and public key

test_that("reading protected keys", {
  sk1 <- rsa_keygen()
  pk1 <- as.list(sk1)$pubkey
  tmp_key <- tempfile()
  tmp_pubkey <- tempfile()
  write_pem(sk1, tmp_key, password = NULL)
  write_pem(pk1, tmp_pubkey)

  # Test default private key
  Sys.setenv("USER_KEY" = tmp_key)
  expect_equal(sk1, my_key())
  expect_equal(pk1, my_pubkey())

  # Test default pubkey key
  Sys.setenv("USER_PUBKEY" = tmp_pubkey)
  expect_equal(pk1, my_pubkey())

  Sys.unsetenv("USER_KEY")
  expect_equal(pk1, my_pubkey())

  # Clean up to prevent side effects
  Sys.unsetenv("USER_PUBKEY")
})
