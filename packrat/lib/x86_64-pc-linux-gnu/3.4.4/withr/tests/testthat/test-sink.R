context("With sink")

test_that("with_output_sink works as expected", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  tmp3 <- tempfile()
  on.exit(unlink(tmp3), add = TRUE)

  expect_identical(sink.number(), 0L)

  with_output_sink(tmp, {
    expect_identical(sink.number(), 1L)
    cat("output\n")
  })
  expect_identical(readLines(tmp), "output")

  expect_identical(sink.number(), 0L)

  with_output_sink(tmp, append = TRUE, {
    expect_identical(sink.number(), 1L)
    cat("output 2\n")
  })
  expect_identical(readLines(tmp), c("output", "output 2"))

  expect_identical(sink.number(), 0L)

  expect_warning(
    with_output_sink(tmp, {
      sink()
    }),
    "already removed"
  )

  expect_identical(sink.number(), 0L)

  expect_error(
    with_output_sink(NULL, {
      NULL
    }),
    "cannot be NULL"
  )

  expect_identical(sink.number(), 0L)

})
