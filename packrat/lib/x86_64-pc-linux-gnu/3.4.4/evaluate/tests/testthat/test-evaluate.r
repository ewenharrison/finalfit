context("Evaluation")

test_that("file with only comments runs", {
  ev <- evaluate(file("comment.r"))
  expect_that(length(ev), equals(2))

  expect_that(classes(ev), equals(c("source", "source")))
})

test_that("data sets loaded", {
  ev <- evaluate(file("data.r"))
  if (require("lattice", quietly = TRUE)) expect_that(length(ev), equals(3))
})

# # Don't know how to implement this
# test_that("newlines escaped correctly", {
#   ev <- evaluate("cat('foo\n')")
#   expect_that(ev[[1]]$src, equals("cat('foo\\n'))"))
# })

test_that("terminal newline not needed", {
  ev <- evaluate("cat('foo')")
  expect_that(length(ev), equals(2))
  expect_that(ev[[2]], equals("foo"))
})

test_that("S4 methods are displayed with show, not print", {
  setClass("A", contains = "function", where = environment())
  setMethod("show", "A", function(object) cat("B"))
  a <- new('A', function() b)

  ev <- evaluate("a")
  expect_equal(ev[[2]], "B")
})

test_that("errors during printing visible values are captured", {
  setClass("A", contains = "function", where = environment())
  setMethod("show", "A", function(object) stop("B"))
  a <- new('A', function() b)

  ev <- evaluate("a")
  stopifnot("error" %in% class(ev[[2]]))
})

test_that("options(warn = -1) suppresses warnings", {
  ev <- evaluate("op = options(warn = -1); warning('hi'); options(op)")
  expect_that(classes(ev), equals("source"))
})

test_that("options(warn = 0) and options(warn = 1) produces warnings", {
  ev <- evaluate("op = options(warn = 0); warning('hi'); options(op)")
  expect_equal(classes(ev), c("source", "simpleWarning"))

  ev <- evaluate("op = options(warn = 1); warning('hi'); options(op)")
  expect_equal(classes(ev), c("source", "simpleWarning"))
})

# See https://github.com/r-lib/evaluate/pull/81#issuecomment-367685196
# test_that("options(warn = 2) produces errors instead of warnings", {
#   ev_warn_2 <- evaluate("op = options(warn = 2); warning('hi'); options(op)")
#   expect_equal(classes(ev_warn_2), c("source", "simpleError"))
# })

test_that("output and plots interleaved correctly", {
  ev <- evaluate(file("interleave-1.r"))
  expect_equal(classes(ev),
               c("source", "character", "recordedplot", "character", "recordedplot"))

  ev <- evaluate(file("interleave-2.r"))
  expect_equal(classes(ev),
               c("source", "recordedplot", "character", "recordedplot", "character"))
})

test_that("return value of value handler inserted directly in output list", {
  ev <- evaluate(file("raw-output.r"), output_handler = new_output_handler(value = identity))
  if (require("ggplot2", quietly = TRUE)) {
    expect_equal(classes(ev),
                 c("source", "numeric", "source", "source", "source", "gg"))
  }
})

test_that("invisible values can also be saved if value handler has two arguments", {
  handler <- new_output_handler(value = function(x, visible) {
    x  # always returns a visible value
  })
  ev <- evaluate("x<-1:10", output_handler = handler)
  expect_equal(classes(ev), c("source", "integer"))
})

test_that("multiple expressions on one line can get printed as expected", {
  ev <- evaluate("x <- 1; y <- 2; x; y")
  expect_equal(classes(ev), c("source", "character", "character"))
})

test_that("multiple lines of comments do not lose the terminating \\n", {
  ev <- evaluate("# foo\n#bar")
  expect_equal(ev[[1]][["src"]], "# foo\n")
})
