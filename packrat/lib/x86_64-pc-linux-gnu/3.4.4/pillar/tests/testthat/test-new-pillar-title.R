context("test-new-pillar-title.R")

test_that("can format new_pillar_title()", {
  expect_equal(
    format(new_pillar_title(names(iris))),
    format_full_pillar_title(names(iris))
  )
  expect_equal(
    format(new_pillar_title(names(mtcars))),
    format_full_pillar_title(names(mtcars))
  )
})

test_that("titles are left-adjusted", {
  expect_equal(
    get_extent(format(new_pillar_title(c("ab", "c")))),
    c(2, 2)
  )
  expect_equal(
    format(new_pillar_title(c("ab", "c"))),
    style_title(c("ab", "c "))
  )
})
