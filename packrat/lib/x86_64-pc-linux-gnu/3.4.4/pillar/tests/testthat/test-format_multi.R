context("format_multi")

withr::with_options(list(pillar.bold = TRUE), {
  test_that("output test", {
    x <- list(column_zero_one = 1:3 + 0.23, col_02 = letters[1:3], col_03 = factor(letters[1:3]), col_04 = ordered(letters[1:3]))
    expect_pillar_output(xf = colonnade(x, width = 4), filename = "multi-04.txt")
    expect_pillar_output(xf = colonnade(x, width = 5), filename = "multi-05.txt")
    expect_pillar_output(xf = colonnade(x, width = 6), filename = "multi-06.txt")
    expect_pillar_output(xf = colonnade(x, width = 7), filename = "multi-07.txt")
    expect_pillar_output(xf = colonnade(x, width = 8), filename = "multi-08.txt")
    expect_pillar_output(xf = colonnade(x, width = 9), filename = "multi-09.txt")
    expect_pillar_output(xf = colonnade(x, width = 10), filename = "multi-10.txt")
    expect_pillar_output(xf = colonnade(x, width = 11), filename = "multi-11.txt")
    expect_pillar_output(xf = colonnade(x, width = 12), filename = "multi-12.txt")
    expect_pillar_output(xf = colonnade(x, width = 13), filename = "multi-13.txt")
    expect_pillar_output(xf = colonnade(x, width = 14), filename = "multi-14.txt")
    expect_pillar_output(xf = colonnade(x, width = 15), filename = "multi-15.txt")
    expect_pillar_output(xf = colonnade(x, width = 16), filename = "multi-16.txt")
    expect_pillar_output(xf = colonnade(x, width = 17), filename = "multi-17.txt")
    expect_pillar_output(xf = colonnade(x, width = 18), filename = "multi-18.txt")
    expect_pillar_output(xf = colonnade(x, width = 19), filename = "multi-19.txt")
    expect_pillar_output(xf = colonnade(x, width = 20), filename = "multi-20.txt")
    expect_pillar_output(xf = colonnade(x, width = 21), filename = "multi-21.txt")
    expect_pillar_output(xf = colonnade(x, width = 22), filename = "multi-22.txt")
    expect_pillar_output(xf = colonnade(x, width = 23), filename = "multi-23.txt")
    expect_pillar_output(xf = colonnade(x, width = 24), filename = "multi-24.txt")
    expect_pillar_output(xf = colonnade(x, width = 25), filename = "multi-25.txt")
    expect_pillar_output(xf = colonnade(x, width = 26), filename = "multi-26.txt")
    expect_pillar_output(xf = colonnade(x, width = 27), filename = "multi-27.txt")
    expect_pillar_output(xf = colonnade(x, width = 28), filename = "multi-28.txt")
    expect_pillar_output(xf = colonnade(x, width = 29), filename = "multi-29.txt")
    expect_pillar_output(xf = colonnade(x, width = 30), filename = "multi-30.txt")
    expect_pillar_output(xf = colonnade(x, width = 31), filename = "multi-31.txt")
    expect_pillar_output(xf = colonnade(x, width = 32), filename = "multi-32.txt")
    expect_pillar_output(xf = colonnade(x, width = 33), filename = "multi-33.txt")
    expect_pillar_output(xf = colonnade(x, width = 34), filename = "multi-34.txt")
    expect_pillar_output(xf = colonnade(x, width = 35), filename = "multi-35.txt")
    expect_pillar_output(xf = colonnade(x, width = 36), filename = "multi-36.txt")
    expect_pillar_output(xf = colonnade(x, width = 37), filename = "multi-37.txt")
    expect_pillar_output(xf = colonnade(x, width = 38), filename = "multi-38.txt")
    expect_pillar_output(xf = colonnade(x, width = 39), filename = "multi-39.txt")
    expect_pillar_output(xf = colonnade(x, width = Inf), filename = "multi-inf.txt")

    expect_pillar_output(
      xf = colonnade(
        rep(list(paste(letters, collapse = " ")), 4),
        width = Inf
      ),
      filename = "letters-inf.txt"
    )

    expect_pillar_output(
      xf = new_vertical(extra_cols(squeeze(colonnade(x), width = 10))),
      filename = "multi-extra-10.txt"
    )

    expect_pillar_output(
      xf = new_vertical(extra_cols(squeeze(colonnade(x), width = 20))),
      filename = "multi-extra-20.txt"
    )

    expect_pillar_output(
      xf = new_vertical(extra_cols(squeeze(colonnade(x), width = 30))),
      filename = "multi-extra-30.txt"
    )

    expect_pillar_output(
      xf = new_vertical(extra_cols(squeeze(colonnade(x), width = 35))),
      filename = "multi-extra-35.txt"
    )

    expect_pillar_output(
      xf = new_vertical(extra_cols(squeeze(colonnade(x), width = 40))),
      filename = "multi-extra-40.txt"
    )
  })

  test_that("tests from tibble", {
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(mtcars[1:8, ], has_row_id = "*", width = 30),
      filename = "tibble-mtcars-8-30.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(iris[1:5, ], width = 30),
      filename = "tibble-iris-5-30.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(iris[1:3, ], width = 20),
      filename = "tibble-iris-3-20.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 30),
      filename = "tibble-all--30.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      filename = "tibble-all--300.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 70L,
      filename = "tibble-all--300-70.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 60L,
      filename = "tibble-all--300-60.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 50L,
      filename = "tibble-all--300-50.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 40L,
      filename = "tibble-all--300-40.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 30L,
      filename = "tibble-all--300-30.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(df_all, width = 300),
      output_width = 20L,
      filename = "tibble-all--300-20.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(list(`\n` = c("\n", '"'), `\r` = factor("\n")), width = 30),
      filename = "tibble-newline.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(list(a = c("", " ", "a ", " a")), width = 30),
      filename = "tibble-space.txt"
    )
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(list("mean(x)" = 5, "var(x)" = 3), width = 30),
      filename = "tibble-non-syntactic.txt"
    )
  })

  test_that("empty", {
    expect_equal(
      format(colonnade(list(a = character(), b = logical()), width = 30)),
      structure(character(), class = "pillar_vertical")
    )
    expect_equal(
      format(colonnade(iris[1:5, character()], width = 30)),
      structure(character(), class = "pillar_vertical")
    )
  })

  test_that("NA names", {
    x <- list(`NA` = 1:3, set_to_NA = 4:6)
    names(x)[[2]] <- NA_character_
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(x, width = 30),
      filename = "na-names.txt"
    )
  })

  test_that("sep argument", {
    x <- list(sep = 1:3)
    expect_pillar_output(
      crayon = FALSE,
      xf = colonnade(x, width = 30),
      filename = "sep.txt"
    )
  })

  test_that("without styling", {
    xf <- colonnade(list(x = (10^(-3:4)) * c(-1, 1)))

    withr::with_options(
      list(),
      expect_pillar_output(
        xf = xf,
        filename = "style-regular.txt"
      )
    )
    withr::with_options(
      list(pillar.subtle_num = TRUE),
      expect_pillar_output(
        xf = xf,
        filename = "style-subtle-num-true.txt"
      )
    )
    withr::with_options(
      list(pillar.subtle = FALSE),
      expect_pillar_output(
        xf = xf,
        filename = "style-subtle-false.txt"
      )
    )
    withr::with_options(
      list(pillar.neg = FALSE),
      expect_pillar_output(
        xf = xf,
        filename = "style-neg-false.txt"
      )
    )
    withr::with_options(
      list(pillar.subtle = FALSE, pillar.neg = FALSE),
      expect_pillar_output(
        xf = xf,
        filename = "style-subtle-neg-false.txt"
      )
    )
    withr::with_options(
      list(pillar.bold = FALSE),
      expect_pillar_output(
        xf = xf,
        filename = "style-bold-false.txt"
      )
    )
  })
}) # withr::with_options(list(pillar.bold = TRUE),
