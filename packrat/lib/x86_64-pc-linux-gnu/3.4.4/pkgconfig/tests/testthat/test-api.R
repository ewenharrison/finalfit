
context("Creating a custom API")

test_that("We can create a custom API", {

  on.exit(try(disposables::dispose_packages(pkgs)))
  pkgs <- disposables::make_packages(

    utility123456 = {
      set_opt <- function(...) {
        pars <- list(...)
        names(pars) <- paste0("utility123456::", names(pars))
        do.call(pkgconfig::set_config_in,
                c(pars, list(.in = parent.frame())))
      }
      
      get_opt <- function(key) {
        real_key <- paste0("utility123456::", key)
        pkgconfig::get_config(real_key)
      }
    },

    pkgA = {
      setter <- function() { utility123456::set_opt(key4 = "value_A") }
      getter <- function() { utility123456::get_opt("key4") }
    },

    pkgB = {
      setter <- function() { utility123456::set_opt(key4 = "value_B") }
      getter <- function() { utility123456::get_opt("key4") }
    }
  )

  pkgA::setter()
  pkgB::setter()

  expect_equal(pkgA::getter(), "value_A")
  expect_equal(pkgB::getter(), "value_B")
  
})
