context("Nonblocking connection")

test_that("Non blocking connections ", {
  h <- new_handle()
  con <- curl(httpbin("drip?duration=3&numbytes=50&code=200"), handle = h)
  expect_equal(handle_data(h)$status_code, 0L)
  open(con, "rb", blocking = FALSE)
  expect_equal(handle_data(h)$status_code, 200L)
  n <- 0
  while(isIncomplete(con)){
    Sys.sleep(0.01)
    buf <- readBin(con, raw(), 5)
    n <- n + length(buf)
  }
  expect_equal(n, 50L)
  rm(h)
  close(con)
  gc()
  expect_equal(total_handles(), 0L)
})

test_that("Non blocking readline", {
  con <- curl(httpbin("stream/71"))
  open(con, "r", blocking = FALSE)
  n <- 0
  while(isIncomplete(con)){
    buf <- readLines(con, 5)
    n <- n + length(buf)
  }
  expect_equal(n, 71L)
  close(con)
  gc()
  expect_equal(total_handles(), 0L)
})

test_that("isIncomplete for blocking connections", {
  con <- curl(httpbin("stream/71"))
  expect_false(isIncomplete(con))
  expect_equal(length(readLines(con)), 71L)
  expect_false(isIncomplete(con))
  open(con)
  expect_true(isIncomplete(con))
  n <- 0
  while(isIncomplete(con)){
    buf <- readLines(con, 5)
    n <- n + length(buf)
  }
  expect_equal(n, 71L)
  close(con)
  gc()
  expect_equal(total_handles(), 0L)
})

test_that("Small buffers", {
  con <- curl(httpbin("get"))
  expect_false(isIncomplete(con = con))
  open(con)
  on.exit(close(con), add = TRUE)
  expect_true(isIncomplete(con = con))
  readLines(con, 1)
  expect_true(isIncomplete(con = con))
  readLines(con)
  expect_false(isIncomplete(con = con))
})
