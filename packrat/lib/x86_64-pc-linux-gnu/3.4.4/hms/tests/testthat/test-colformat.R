context("pillar")

test_that("pillar", {
  expect_known_pillar_shaft_display(
    hms(c(-3600, -60, -1, -0.001, 0, 0.001, 1, 60, 3600, NA)),
    file = "hmss.txt"
  )
  expect_known_pillar_shaft_display(
    hms(c(-3600000, -3600, -60, -1, 0, 1, 60, 3600, 3600000, NA)),
    file = "hms.txt"
  )
  expect_known_pillar_shaft_display(
    hms(c(-3600, -60, 0, 60, 3600, NA)),
    file = "hm.txt"
  )
  expect_known_pillar_shaft_display(
    hms(c(-60, -1, 0, 1, 60, NA)),
    file = "ms.txt"
  )
  expect_known_pillar_shaft_display(
    hms(c(-60, -1, -0.001, 0, 0.001, 1, 60, NA)),
    file = "mss.txt"
  )
})
