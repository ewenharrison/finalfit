context("Non-blocking opening connection")

read_text <- function(x){
  while (isIncomplete(x)) {
    Sys.sleep(0.1)
    txt <- readLines(x)
    if(length(txt))
      return(txt)
  }
}

read_bin <- function(x){
  while (isIncomplete(x)) {
    Sys.sleep(0.1)
    bin <- readBin(x, raw(), 100)
    if(length(bin))
      return(bin)
  }
}

expect_immediate <- function(...){
  expect_true(system.time(...)['elapsed'] < 0.5)
}

test_that("Non-blocking open does not block", {

  # Get a regular string
  con <- curl(httpbin("delay/1"))
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_immediate(readLines(con))
  close(con)
})

test_that("Error handling for non-blocking open", {

  # Get a regular string
  con <- curl(httpbin("get"))
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_is(read_text(con), "character")
  close(con)

  # Test error during read text
  h <- new_handle()
  con <- curl(httpbin("status/418"), handle = h)
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_is(read_text(con), "character")
  expect_equal(handle_data(h)$status_code, 418)
  close(con)

  # Test error during read binary
  h <- new_handle()
  con <- curl(httpbin("status/418"), handle = h)
  expect_immediate(open(con, "rbs", blocking = FALSE))
  expect_is(read_bin(con), "raw")
  expect_equal(handle_data(h)$status_code, 418)
  close(con)

  # DNS error
  con <- curl("http://this.is.invalid.co.za")
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_error(read_text(con), "resolve")
  close(con)

  # Non existing host
  con <- curl("http://240.0.0.1")
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_error(read_text(con))
  close(con)

  # Invalid port
  con <- curl("http://8.8.8.8:666")
  expect_immediate(open(con, "rs", blocking = FALSE))
  expect_error(read_text(con))
  close(con)
})
