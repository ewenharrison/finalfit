context("Read certificates")

test_that("Roundtrip pem/der formats", {
  cert <- read_cert("../keys/opencpu.org.cer", der = TRUE)
  expect_equal(cert, read_cert(write_pem(cert)))
  expect_equal(cert, read_cert(write_pem(cert, tempfile())))
  expect_equal(cert, read_cert(write_der(cert), der = TRUE))
  expect_equal(cert, read_cert(write_der(cert, tempfile()), der = TRUE))
})

