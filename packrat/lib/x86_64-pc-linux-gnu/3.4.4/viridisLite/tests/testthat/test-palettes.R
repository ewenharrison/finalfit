context("palette generation")
test_that("palette generation is accurate", {

  # data is intact
  expect_equal(dim(viridis.map), c(1280, 4))

  # options work as expected
  expect_equal(viridis(1, option = "D"), "#440154FF")
  expect_equal(viridis(1, alpha = 0, option = "D"), "#44015400")
  expect_equal(viridis(1, begin = 1, end = 0, option = "D"), "#FDE725FF")
  expect_equal(viridis(1, alpha = 0, begin = 1, end = 0, option = "D"), "#FDE72500")
  expect_equal(viridis(2, alpha = 0, begin = 1, end = 0, direction = -1, option = "D"),
               c("#44015400", "#FDE72500"))

  # options work as expected
  expect_equal(viridis(1, option = "C"), "#0D0887FF")
  expect_equal(viridis(1, alpha = 0, option = "C"), "#0D088700")
  expect_equal(viridis(1, begin = 1, end = 0, option = "C"), "#F0F921FF")
  expect_equal(viridis(1, alpha = 0, begin = 1, end = 0, option = "C"), "#F0F92100")
  expect_equal(viridis(2, alpha = 0, begin = 1, end = 0, direction = -1, option = "C"),
               c("#0D088700", "#F0F92100"))

  # options work as expected
  expect_equal(viridis(1, option = "B"), "#000004FF")
  expect_equal(viridis(1, alpha = 0, option = "B"), "#00000400")
  expect_equal(viridis(1, begin = 1, end = 0, option = "B"), "#FCFFA4FF")
  expect_equal(viridis(1, alpha = 0, begin = 1, end = 0, option = "B"), "#FCFFA400")
  expect_equal(viridis(2, alpha = 0, begin = 1, end = 0, direction = -1, option = "B"),
               c("#00000400", "#FCFFA400"))

  # options work as expected
  expect_equal(viridis(1, option = "A"), "#000004FF")
  expect_equal(viridis(1, alpha = 0, option = "A"), "#00000400")
  expect_equal(viridis(1, begin = 1, end = 0, option = "A"), "#FCFDBFFF")
  expect_equal(viridis(1, alpha = 0, begin = 1, end = 0, option = "A"), "#FCFDBF00")
  expect_equal(viridis(2, alpha = 0, begin = 1, end = 0, direction = -1, option = "A"),
               c("#00000400", "#FCFDBF00"))

  # options work as expected
  expect_equal(viridis(1, option = "E"), "#00204DFF")
  expect_equal(viridis(1, alpha = 0, option = "E"), "#00204D00")
  expect_equal(viridis(1, begin = 1, end = 0, option = "E"), "#FFEA46FF")
  expect_equal(viridis(1, alpha = 0, begin = 1, end = 0, option = "E"), "#FFEA4600")
  expect_equal(viridis(2, alpha = 0, begin = 1, end = 0, direction = -1, option = "E"),
              c("#00204D00", "#FFEA4600"))

  # bad inputs
  expect_warning(viridis(1, option = "F"))
  expect_error(viridis(1, direction=100))
  expect_error(viridis(1, begin = -1))
  expect_error(viridis(1, begin = 100))
  expect_error(viridis(1, end = -1))
  expect_error(viridis(1, end = 100))

  # we've already proven these work with ^^ but we'll add a few more values to
  # generate and get better code coverage this way.

  expect_equal(magma(3), c("#000004FF", "#B63679FF", "#FCFDBFFF"))
  expect_equal(inferno(3), c("#000004FF", "#BB3754FF", "#FCFFA4FF"))
  expect_equal(plasma(3), c("#0D0887FF", "#CC4678FF", "#F0F921FF"))
  expect_equal(cividis(3), c("#00204DFF", "#7C7B78FF", "#FFEA46FF"))

})
