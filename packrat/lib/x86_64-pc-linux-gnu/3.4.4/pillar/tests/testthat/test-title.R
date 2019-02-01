context("title")

withr::with_options(list(pillar.bold = TRUE), {

  test_that("with and without title", {
    expect_pillar_output(10^(1:6), filename = "title-none.txt", crayon = FALSE)
    expect_pillar_output(10^(1:6), title = "crayon", filename = "title-crayon.txt")
    expect_pillar_output(10^(1:6), title = "short", filename = "title-short.txt", crayon = FALSE)
    expect_pillar_output(10^(1:6), title = "somewhat_wider", filename = "title-longer.txt", crayon = FALSE)
    expect_pillar_output(10^(1:6), title = "exactly_fifteen", filename = "title-fifteen.txt", crayon = FALSE)
    expect_pillar_output(10^(1:6), title = "absolutely_breaking_all_sensible_boundaries", width = 18, filename = "title-too-long.txt", crayon = FALSE)
  })
}) # withr::with_options(list(pillar.bold = TRUE), {
