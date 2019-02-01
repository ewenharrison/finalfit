context("Multi handle")

test_that("Max connections works", {
  skip_on_os("solaris")
  skip_if_not(strsplit(curl_version()$version, "-")[[1]][1] >= as.numeric_version("7.30"),
    "libcurl does not support host_connections")
  multi_set(host_con = 2, multiplex = FALSE)
  for(i in 1:3){
    multi_add(new_handle(url = httpbin("delay/2")))
  }
  out <- multi_run(timeout = 3.5)
  expect_equal(out, list(success = 2, error = 0, pending = 1))
  out <- multi_run(timeout = 2)
  expect_equal(out, list(success = 1, error = 0, pending = 0))
  out <- multi_run()
  expect_equal(out, list(success = 0, error = 0, pending = 0))
})

test_that("Max connections reset", {
  skip_on_os("solaris")
  multi_set(host_con = 6, multiplex = TRUE)
  for(i in 1:3){
    multi_add(new_handle(url = httpbin("delay/2")))
  }
  out <- multi_run(timeout = 4)
  expect_equal(out, list(success = 3, error = 0, pending = 0))
})

test_that("Timeout works", {
  skip_on_os("solaris")
  h1 <- new_handle(url = httpbin("delay/3"))
  h2 <- new_handle(url = httpbin("post"), postfields = "bla bla")
  h3 <- new_handle(url = "https://urldoesnotexist.xyz", connecttimeout = 1)
  h4 <- new_handle(url = "http://localhost:14", connecttimeout = 1)
  m <- new_pool()
  multi_add(h1, pool = m)
  multi_add(h2, pool = m)
  multi_add(h3, pool = m)
  multi_add(h4, pool = m)
  rm(h1, h2, h3, h4)
  gc()
  out <- multi_run(timeout = 2, pool = m)
  expect_equal(out, list(success = 1, error = 2, pending = 1))
  out <- multi_run(timeout = 0, pool = m)
  expect_equal(out, list(success = 0, error = 0, pending = 1))
  out <- multi_run(pool = m)
  expect_equal(out, list(success = 1, error = 0, pending = 0))
})

test_that("Callbacks work", {
  total = 0;
  h1 <- new_handle(url = httpbin("get"))
  multi_add(h1, done = function(...){
    total <<- total + 1
    multi_add(h1, done = function(...){
      total <<- total + 1
    })
  })
  gc() # test that callback functions are protected
  out <- multi_run()
  expect_equal(out, list(success=2, error=0, pending=0))
  expect_equal(total, 2)
})

test_that("Multi cancel works", {
  expect_length(multi_list(), 0)
  h1 <- new_handle(url = httpbin("get"))
  multi_add(h1)
  expect_length(multi_list(), 1)
  expect_error(multi_add(h1), "locked")
  expect_equal(multi_run(timeout = 0), list(success = 0, error = 0, pending = 1))
  expect_length(multi_list(), 1)
  expect_is(multi_cancel(h1), "curl_handle")
  expect_length(multi_list(), 0)
  expect_is(multi_add(h1), "curl_handle")
  expect_length(multi_list(), 1)
  expect_equal(multi_run(), list(success = 1, error = 0, pending = 0))
  expect_length(multi_list(), 0)
})

test_that("Errors in Callbacks", {
  pool <- new_pool()
  cb <- function(req){
    stop("testerror in callback!")
  }
  curl_fetch_multi(httpbin("get"), pool = pool, done = cb)
  curl_fetch_multi(httpbin("status/404"), pool = pool, done = cb)
  curl_fetch_multi("https://urldoesnotexist.xyz", pool = pool, fail = cb)
  gc()
  expect_equal(total_handles(), 3)
  expect_error(multi_run(pool = pool), "testerror")
  gc()
  expect_equal(total_handles(), 2)
  expect_error(multi_run(pool = pool), "testerror")
  gc()
  expect_equal(total_handles(), 1)
  expect_error(multi_run(pool = pool), "testerror")
  gc()
  expect_equal(total_handles(), 0)
  expect_equal(multi_run(pool = pool), list(success = 0, error = 0, pending = 0))
})

test_that("Data callback", {
  con <- rawConnection(raw(0), "r+")
  on.exit(close(con))
  hx <- new_handle()
  handle_setopt(hx, COPYPOSTFIELDS = jsonlite::toJSON(mtcars));
  handle_setheaders(hx, "Content-Type" = "application/json")
  status <- NULL
  curl_fetch_multi(httpbin("post"), done = function(res){
    status <<- res$status_code
  }, fail = stop, data = function(x){
    writeBin(x, con)
  }, handle = hx)

  curl_fetch_multi(httpbin("get"), done = function(res){
    #this somehow breaks the gc
    #expect_equal(res$status_code, 200)
  }, fail = stop, data = function(x){
    expect_is(x, "raw")
  })

  # test protect
  gc()

  # perform requests
  out <- multi_run()
  expect_equal(out$success, 2)
  expect_equal(status, 200)

  # get data from buffer
  content <- rawConnectionValue(con)
  output <- jsonlite::fromJSON(rawToChar(content))
  expect_is(output$json, "data.frame")
  expect_equal(sort(names(output$json)), sort(names(mtcars)))
})

test_that("callback protection", {
  done <- function(res){
    expect_is(res$status_code, "integer")
  }
  fail <- function(...){
    print("error")
  }
  data <- function(x){
    expect_is(x, "raw")
  }
  pool <- new_pool()
  handle <- new_handle(url = httpbin("get"))
  multi_add(handle, done = done, fail = fail, data = data, pool = pool)
  rm(handle, done, fail, data)
  gc(); gc();
  out <- multi_run(pool = pool)
  expect_equal(out$success, 1)
})

test_that("host_con works via and multi_fdset", {
  pool <- new_pool(host_con = 3)
  for (i in 4:0) {
    h1 <- new_handle(url = httpbin(paste0("delay/", i)))
    multi_add(h1, done = force, fail = cat, pool = pool)
  }
  for(i in 4:0){
    res <- multi_run(pool = pool, poll = 1)
    expect_length(multi_fdset(pool = pool)$reads, min(3, i))
  }
})

test_that("GC works", {
  gc()
  expect_equal(total_handles(), 0L)
})


