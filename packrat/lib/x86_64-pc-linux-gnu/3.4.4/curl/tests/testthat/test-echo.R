context("echo")

test_that("echo large data", {
  bindata <- serialize(rnorm(1e5), NULL)
  handle <- curl::new_handle()
  curl::handle_setform(handle, data =  curl::form_data(bindata, "application/octet-stream"))
  formdata <- curl::curl_echo(handle = handle)
  out <- webutils::parse_http(formdata$body, formdata$content_type)
  expect_identical(out$data$value, bindata)
})
