context("URL escaping")

test_that("basic encoding", {
  expect_equal("a%2Fb%2Fc", curl_escape("a/b/c"))
  expect_equal("a = b + c", curl_unescape("a%20%3D%20b%20%2B%20c"))
})

test_that("curl_{,un}escape handle NULL", {
  escaped_null <- curl_escape(NULL)
  expect_equal(0, length(escaped_null))
  expect_equal("character", class(escaped_null))
  unescaped_null <- curl_unescape(NULL)
  expect_equal(0, length(unescaped_null))
  expect_equal("character", class(unescaped_null))
})

test_that("curl_escape and curl_unescape are inverses", {
  mu <- "\u00b5"
  expect_equal(mu, curl_unescape(curl_escape(mu)))
  escaped_mu <- curl_escape(mu)
  expect_equal(escaped_mu, curl_escape(curl_unescape(escaped_mu)))
})

test_that("Test character encoding", {
  strings <- c(
    "Zürich",
    "北京填鴨们",
    "ผัดไทย",
    "寿司",
    rawToChar(as.raw(1:40)),
    "?foo&bar=baz!bla\n"
  )
  strings <- enc2utf8(strings)
  expect_equal(strings, curl_unescape(curl_escape(strings)))
})
