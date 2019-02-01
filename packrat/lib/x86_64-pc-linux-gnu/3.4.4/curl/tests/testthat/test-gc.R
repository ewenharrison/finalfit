context("Garbage collection")

gc()
h1 <- new_handle()
test <- function(){
  pool <- new_pool()
  h2 <- new_handle()
  cb <- function(...){}
  curl_fetch_multi('http://jeroen.github.io/images/frink.png', pool = pool, done = cb, handle = h1)
  curl_fetch_multi('http://jeroen.github.io/images/frink.png', pool = pool, done = cb, handle = h2)
  return(pool)
}

test_that("Garbage collection works", {
  # Should clean 0 handles
  pool <- test()
  expect_equal(total_handles(), 2L)
  multi_run(pool = pool)
  gc()
  expect_equal(total_handles(), 1L)
})

rm(h1)

test_that("Garbage collection works", {
  gc()
  expect_equal(total_handles(), 0L)
})

# Test circular GC problems
test2 <- function(){
  pool <- new_pool()
  cb <- function(...){}
  curl_fetch_multi('http://jeroen.github.io/images/frink.png', pool = pool, done = cb)
  curl_fetch_multi('http://jeroen.github.io/images/frink.png', pool = pool, done = cb)
}

test_that("Clean up pending requets", {
  test2()
  gc()
  expect_equal(total_handles(), 0L)
})

# Test3 circular GC problems
test3 <- function(){
  pool <- new_pool()
  curl_fetch_multi('https://cran.r-project.org/src/contrib/stringi_1.1.1.tar.gz', pool = pool)
  curl_fetch_multi('https://cran.r-project.org/src/contrib/stringi_1.1.1.tar.gz', pool = pool)
  return(pool)
}

test_that("Clean up hanging requests", {
  pool <- test3()
  expect_equal(total_handles(), 2L)
  multi_run(0, pool = pool)
  rm(pool)
  gc()
  expect_equal(total_handles(), 0L)
})


