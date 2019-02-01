context("IDN")

test_that("IDN domain names",{
  # OSX does not support IDN by default :(
  skip_if_not(curl_version()$idn, "libcurl does not have libidn")
  skip_on_cran()

  koln <- "http://www.köln.de"
  expect_is(curl::curl_fetch_memory(enc2utf8(koln))$status_code, "integer")
  expect_is(curl::curl_fetch_memory(enc2native(koln))$status_code, "integer")

  con <- curl::curl(enc2utf8(koln))
  expect_is(readLines(con, warn = FALSE), "character")
  close(con)

  con <- curl::curl(enc2native(koln))
  expect_is(readLines(con, warn = FALSE), "character")
  close(con)

  kremlin <- "http://\u043F\u0440\u0435\u0437\u0438\u0434\u0435\u043D\u0442.\u0440\u0444"
  expect_is(curl::curl_fetch_memory(kremlin)$status_code, "integer")

  con <- curl::curl(kremlin)
  expect_is(readLines(con, warn = FALSE), "character")
  close(con)

  # Something random that doesn't exist
  wrong <- "http://\u043F\u0440\u00F6\u0437\u0438\u0434\u0435\u043D\u0442.\u0440\u0444"
  expect_error(curl::curl_fetch_memory(enc2utf8(wrong)), 'resolve')
})
