
context("Global env")

test_that("Global env does not bother packages", {

  evalq(set_config(key3 = "value"), .GlobalEnv)
  on.exit(try(evalq(set_config(key3 = NULL), .GlobalEnv)), add = TRUE)

  on.exit(try(disposables::dispose_packages(pkgs)), add = TRUE)

  pkgs <- disposables::make_packages(
    pkgA = {
      setter <- function() { set_config(key3 = "value2") }
      getter <- function() { utility123456::getter() }
    },
    utility123456 = {
      getter <- function() { get_config("key3", "fallback") }
    }
  )

  pkgA::setter()

  expect_equal(get_config("key3"), "value")
  expect_equal(pkgA::getter(), "value2")
})
