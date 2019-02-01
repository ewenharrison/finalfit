context("test-new-pillar-type.R")

test_that("can format new_pillar_type()", {
  expect_equal(
    format(new_pillar_type(1:3)),
    format_full_pillar_type(1:3)
  )
  expect_equal(
    format(new_pillar_type(list(1, 2:3))),
    format_full_pillar_type(list(1, 2:3))
  )
})
