context("Certificate validation")

# This tests TLS 1.2
test_that("CloudFlare / LetsEncrypt certs", {
  expect_equal(curl_fetch_memory('https://www.opencpu.org')$status_code, 200)
  expect_equal(curl_fetch_memory('https://rud.is')$status_code, 200)

  # Test HTTP -> HTTPS (TLS 1.2) redirection
  expect_equal(curl_fetch_memory('http://curl.haxx.se')$status_code, 200)
})

test_that("Invalid domain raises an error", {
  ipaddr <- nslookup("www.google.com", ipv4_only = TRUE)
  fake_url <- paste0("https://", ipaddr)
  expect_error(curl_fetch_memory(fake_url), "certificate")
  expect_is(curl_fetch_memory(fake_url, handle = new_handle(ssl_verifyhost = FALSE))$status, "integer")
})

test_that("GC works", {
  gc()
  expect_equal(total_handles(), 0L)
})
