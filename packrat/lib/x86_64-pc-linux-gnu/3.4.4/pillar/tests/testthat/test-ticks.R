context("ticks")

test_that("title ticks without width restriction", {
  expect_equal(format_title("proper_title", Inf), "proper_title")
  expect_equal(format_title("needs ticks", Inf), "`needs ticks`")
  expect_equal(format_title("'ticks'", Inf), "`'ticks'`")
  expect_equal(format_title("embedded\nnewline", Inf), "`embedded\\nnewline`")
})

test_that("title ticks and width", {
  expect_equal(format_title("proper_title", 15), "proper_title")
  expect_equal(format_title("proper_title", 12), "proper_title")
  expect_equal(format_title("proper_title", 10), continue("proper_ti"))
  expect_equal(format_title("a b", 6), "`a b`")
  expect_equal(format_title("a b", 5), "`a b`")
  expect_equal(format_title("a b", 4), continue("`a "))
  expect_equal(format_title("a b", 3), continue("`a"))
})
