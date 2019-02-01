
context("Session")

test_that("Session variables", {

  on.exit(try(disposables::dispose_packages(pkgs)))
  pkgs <- disposables::make_packages(
    pkgconfigtest = {
      f <- function() {
        set_config(foo = "bar")
        get_config("foo")
      }
      g <- function() { get_config("foo") }
      h <- function() { get_config("foobar") }
    }
  )

  expect_equal(f(), "bar")
  expect_equal(g(), "bar")
  expect_null(h())

})


test_that("Composite values", {

  on.exit(try(disposables::dispose_packages(pkgs)))
  pkgs <- disposables::make_packages(
    pkgconfigtest = {
      f <- function() {
        set_config(foo = list(1,2,3))
        get_config("foo")
      }
      g <- function() { get_config("foo") }
      h <- function() { get_config("foobar") }
    }
  )

  expect_equal(f(), list(1,2,3))
  expect_equal(g(), list(1,2,3))
  expect_null(h())

})

context("Keys are private")

test_that("Two packages do not interfere", {

  on.exit(try(disposables::dispose_packages(pkgs)))

  pkgs <- disposables::make_packages(

    pkgA = {
      setter <- function() { pkgconfig::set_config(key = "A") }
      getter <- function() { utility123456::getter() }
    },

    pkgB = {
      setter <- function() { pkgconfig::set_config(key = "B") }
      getter <- function() { utility123456::getter() }
    },

    utility123456 = {
      getter <- function() { pkgconfig::get_config("key") }
    }
  )

  pkgA::setter()
  pkgB::setter()

  expect_equal(pkgA::getter(), "A")
  expect_equal(pkgB::getter(), "B")

})

test_that("Cannot get if set by another package", {

  on.exit(try(disposables::dispose_packages(pkgs)))

  pkgs <- disposables::make_packages(
    pkgconfigtest1 = {
      getter <- function() { get_config("foo") }
      getter_parent <- function() { getter() }
    },
    pkgconfigtest2 = {
      setter <- function() { set_config(foo = "bar") }
    }
  )

  pkgconfigtest2::setter()
  expect_null(pkgconfigtest1::getter_parent())
})

test_that("Setting from .onLoad works fine", {

  on.exit(try(disposables::dispose_packages(pkgs)), add = TRUE)

  pkgs <- disposables::make_packages(
    utility123456 = {
      getter <- function() { pkgconfig::get_config("key", "fallback") }
    },

    pkgA = {
      .onLoad <- function(lib, pkg) { pkgconfig::set_config(key = "A") }
      getter <- function() { utility123456::getter() }
    },

    pkgB = {
      .onLoad <- function(lib, pkg) { pkgconfig::set_config(key = "B") }
      getter <- function() { utility123456::getter() }
    },

    pkgC = {
      getter <- function() { utility123456::getter() }
    }
  )

  expect_equal(pkgA::getter(), "A")
  expect_equal(pkgB::getter(), "B")
  expect_equal(pkgC::getter(), "fallback")

})
