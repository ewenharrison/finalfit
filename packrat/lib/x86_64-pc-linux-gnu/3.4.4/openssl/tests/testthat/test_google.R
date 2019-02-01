context("Google SSL tests")

# Certificates from https://pki.goog/

test_that("google certs validate", {
  # Google CA root certs
  gtsr1 <- read_cert('https://pki.goog/gtsr1/GTSR1.crt', der = TRUE)
  gtsr2 <- read_cert('https://pki.goog/gtsr2/GTSR2.crt', der = TRUE)
  gtsr3 <- read_cert('https://pki.goog/gtsr3/GTSR3.crt', der = TRUE)
  gtsr4 <- read_cert('https://pki.goog/gtsr4/GTSR4.crt', der = TRUE)
  gsr2 <- read_cert('https://pki.goog/gsr2/GSR2.crt', der = TRUE)
  gsr4 <- read_cert('https://pki.goog/gsr4/GSR4.crt', der = TRUE)

  # Test good servers
  expect_true(cert_verify(download_ssl_cert('good.r1demo.pki.goog'), gtsr1))
  expect_true(cert_verify(download_ssl_cert('good.r2demo.pki.goog'), gtsr2))
  expect_true(cert_verify(download_ssl_cert('good.r3demo.pki.goog'), gtsr3))
  expect_true(cert_verify(download_ssl_cert('good.r4demo.pki.goog'), gtsr4))
  expect_true(cert_verify(download_ssl_cert('good.gsr2demo.pki.goog'), gsr2))
  expect_true(cert_verify(download_ssl_cert('good.gsr4demo.pki.goog'), gsr4))

  # Test expired servers
  expect_error(cert_verify(download_ssl_cert('expired.r1demo.pki.goog'), gtsr1), 'expired')
  expect_error(cert_verify(download_ssl_cert('expired.r2demo.pki.goog'), gtsr2), 'expired')
  expect_error(cert_verify(download_ssl_cert('expired.r3demo.pki.goog'), gtsr3), 'expired')
  expect_error(cert_verify(download_ssl_cert('expired.r4demo.pki.goog'), gtsr4), 'expired')
  expect_error(cert_verify(download_ssl_cert('expired.gsr2demo.pki.goog'), gsr2), 'expired')
  expect_error(cert_verify(download_ssl_cert('expired.gsr4demo.pki.goog'), gsr4), 'expired')

})
