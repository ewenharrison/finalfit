context("format_rowid")

test_that("output test", {
  expect_pillar_output(
    xf = rowidformat(3),
    filename = "rowid-3.txt"
  )
  expect_pillar_output(
    xf = rowidformat(12, has_title = TRUE, has_star = TRUE),
    filename = "rowid-star-title-12.txt"
  )
})
